(nullify (equal x y))

(defun get-rand-dir ()
  "returns a random pair of integers of either 1, -1 or 0 value"
  (list (1- (random 3)) (1- (random 3))))

(defmacro set-ai (ship action-chooser direction-chooser)
  "assigns higher level controlling behavior to each ship
  (the decision-making part"
  `(setf (ship-ai ,ship) 
	(lambda () (case ,action-chooser 
		     (0 (move ,ship ,direction-chooser))
		     (1 (if (funcall (ship-attack ,ship)) 'hit 'miss))
		     (2 (peek ,ship))
		     (3 'quit)))))

(defun setup-human-ai (this-ship);uses functions from subship-interface.lisp
  "for human players"
  (set-ai this-ship 
	  (get-selection '("move" "attack" "peek" "quit"))
	  (get-direction (prompt-direction))))

(defun setup-trivial-ai (this-ship)
  "totally random behavior"
  (set-ai this-ship (random 3) (get-rand-dir)))

;create a memory of the last time an opponent was seen
(defmacro with-memory (name ship &body body)
  (with-gensyms (turns-since-peek last-enemy-location last-enemy-direction)
    `(let ((,turns-since-peek 0)
	   (,last-enemy-location (get-location ,ship))
	   (,last-enemy-direction (get-direction ,ship)))
       (labels ((,(reread "update-" name) (enemy-ship this-ship)
		  (if (or (ship-seen enemy-ship)
			  (ship-peeking this-ship))
		      (progn 
			(setq ,turns-since-peek 0)
			(setq ,last-enemy-location (get-location enemy-ship))
			(setq ,last-enemy-direction (get-direction enemy-ship)))
		      (incf ,turns-since-peek)))
		(,(reread "get-" name) (what)
		  (case what
		    ('last-location (identity ,last-enemy-location))
		    ('last-direction (identity ,last-enemy-direction))
		    ('turns-since (identity ,turns-since-peek)))))
	 ,@body))))

;end memory stuff

(defmacro setup-simple-ai (this-ship enemy-ship time-tolerance)
  "this one has a memory"
  (with-memory enemy enemy-ship
     (labels ((choose-direction ()
		(list (get-sign (- (car (get-enemy 'last-location)) (ship-x this-ship)))
		      (get-sign (- (cadr (get-enemy 'last-location)) (ship-y this-ship)))))
	      (choose-action ()
		(update-enemy enemy-ship this-ship)
		(cond ((and (equal (get-location this-ship) (get-enemy 'last-location))
			    (zerop (get-enemy 'turns-since)))
		       1)
		      ((> (get-enemy 'turns-since) time-tolerance) 2)
		      (t 0))))
       (set-ai this-ship (choose-action) (choose-direction)))))


;;seting up an ai that has a more sophisticated understanding of the world
;;that is able to deduce things

;the goal with these next few functions is to form a map of squares
;demonstrating the probability of the enemy being any one place
(defun get-search-space (last-seen time-since)
  "receives the last known location of a ship and the time since it has been seen
  and returns a map of the area it could have spread to coupled with values that
  can be assigned probabilities"
  (values
   (loop-over-collect (x y) 
      ((- (first last-seen) time-since) (- (second last-seen) time-since))
      ((+ (first last-seen) time-since 1) (+ (second last-seen) time-since 1))
      (list (if (equal `(,x ,y) last-seen) 1 0) x y))
   (expt (+ 1 (* time-since 2)) 2)
   (* time-since 2)))

(defun set-chance-map-zero (chance-map)
  "sets all probabilities to zero"
  (mapcar #'(lambda (row) 
	      (mapcar #'(lambda (item)
			  (cons 0 (cdr item))) 
		      row))
	  chance-map))

(defun get-next-map (old-map limit)
  "iterates to the next chance map"
  (let ((new-map (set-chance-map-zero (copy-list old-map))))
    (labels ((add-to-neighbors (x y)
	       (let* ((sqr (nth x (nth y old-map)))
		      (val (/ (car sqr) 8)))
		 (loop-over (a b) 
		    ((1- x) (1- y)) 
		    ((+ x 2) (+ y 2))
		    (if (!equal `(,x ,y) `(,a ,b))
			(incf (car (nth a (nth b new-map))) val))))))
      (loop-over (x y) (1 1) (limit limit)
	 (add-to-neighbors x y))
      (identity new-map))))

(defun get-chance-map (turns-since last-seen)
  "receive a location and time and returns a map of probabilities"
  (multiple-value-bind (map area limit) (get-search-space last-seen turns-since)
    (labels ((rec (rmap time)
	       (if (zerop time) rmap 
		   (rec (get-next-map rmap limit) (1- time)))))
      (rec map turns-since))))

(defun reduce-map-to-list (map)
  (if (endp map) nil
      (append (car map) (reduce-map-to-list (cdr map)))))

(defun test-chance-map (map)
  "the sum of probabilities should equal 1"
  (reduce #'+ (mapcar #'car (reduce-map-to-list map))))

(defun print-map (map)
  (format t "~{~a~%~}~%" map))

(defun get-chance-from-map (location map)
  (let ((val (find location (reduce-map-to-list map)
		   :test #'(lambda (x y) (equal x (cdr y))))))
    (if val (car val) 0)))

(defun chance-to-be-here (location last-seen turns-since)
  (let ((chance-map (get-chance-map turns-since last-seen)))
    (get-chance-from-map location chance-map)))

(defun chance-to-hit-from-map (attacker map)
  "calculates the chance to from a premade map"
  (let ((chance 0))
    (dolist (y map)
      (dolist (x y)
	(if (attack attacker 
		    (list (make-instance 'ship 
					 :x (cadr x) 
					 :y (caddr x))))
	    (incf chance (car x)))))
    (identity chance)))

(defun chance-to-hit (attacker target turns-since)
  "calculates the chance to hit from scratch"
  (let ((chance-map (get-chance-map turns-since target)))
    (chance-to-hit-from-map attacker chance-map)))

(defun chance-to-have-come-from (location prev-location prev-map)
  (/ (get-chance-from-map prev-location prev-map) 8))

(defun chance-to-be-hit (defender prev-map attack-function)
  "computes the chance for 'defender' to get hit by a ship with 'attack-function'"
  (let ((prev-map-list (reduce-map-to-list prev-map))
	(chance 0))
    (dolist (loc prev-map-list)
      (let ((x (cadr loc)) (y (caddr loc)))
	(loop-over (a b) 
	   ((1- x) (1- y)) 
	   ((+ x 2) (+ y 2))
	   (if (!equal `(,x ,y) `(,a ,b))
	       (if (funcall attack-function (list 
					     (make-instance 'ship 
							    :x a :y b
							    :a x :b y)
					     defender))
		   (incf chance (/ (car loc) 8)))))))
    (identity chance)))

(defmacro create-chance-map (name scope)
  "creates a global chance map and defines an interface for it"
  `(let ((chance-map nil)
	 (prev-chance-map nil)
	 (limit (* ,scope 2)))
     (defun ,(reread "reset-" name) (location) 
       (setq chance-map (get-search-space location ,scope)))
     (defun ,(reread "increment-" name) () 
       (setq prev-chance-map chance-map)
       (setq chance-map (get-next-map chance-map limit)))
     (defun ,(reread "get-" name) ()
       (identity chance-map))
     (defun ,(reread "prev-" name) ()
       (identity prev-chance-map))
     (defun ,(reread "print-" name) ()
       (print-map (,(reread "get-" name))))))

(defmacro with-chance-map (name scope &body body)
  "creates a local chance map and defines an interface for it"
  (with-gensyms (chance-map prev-map limit)
    `(let ((,chance-map nil)
	   (,prev-map nil)
	   (,limit (* ,scope 2)))
       (labels ((,(reread "reset-" name) (location)
		  (setq ,chance-map (get-search-space location ,scope))) 
		(,(reread "increment-" name) ()
		  (setq ,prev-map ,chance-map)
		  (setq ,chance-map (get-next-map ,chance-map ,limit)))
		(,(reread "get-" name) ()
		  (identity ,chance-map))
		(,(reread "prev-" name) ()
		  (identity ,prev-map))
		(,(reread "print-" name) ()
		  (print-map (,(reread "get-" name)))))
	 ,@body))))

;end chance-map stuff

(with-chance-map map 5
   (chance-to-hit-from-map *destroyer* (ship-attack *destroyer*) (get-map)))

(defun setup-harder-ai (this-ship enemy-ship time-tolerance to-hit-quota)
  (with-memory enemy enemy-ship
      (with-chance-map map time-tolerance
	 (labels ((choose-direction ()
		    (let ((l (get-location this-ship)))
		      (mapcar #'-
			      (cdr 
			       (greatest 
				(group (flatten (loop-over-collect (a b)
				   ((1- (car l)) (1- (cadr l)))
				   ((+ (car l) 2) (+ (cadr l) 2))
				   (let ((cth (chance-to-hit-from-map this-ship 
								      (get-map)))
					 (ctbh (chance-to-be-hit this-ship 
								 (prev-map) 
								 (attack-function enemy-ship))))
				     (list (if (zerop ctbh) cth 
					       (/ cth ctbh)) a b)))) 3)
				   :test #'(lambda (x y) (> (car x) (car y)))))
			      l)))
		  (choose-action ()
		    (if (zerop (get-enemy 'turns-since)) (reset-map (get-enemy 'last-location))
			(increment-map))
		    (update-enemy enemy-ship this-ship)
		    (cond ((> (get-enemy 'turns-since) time-tolerance) 2)
			  ((> (chance-to-hit-from-map this-ship
						      (get-map))
			      to-hit-quota) 1)
			  (t 0))))
	   (reset-map (get-enemy 'last-location))
	   (set-ai this-ship (choose-action) (choose-direction))))))

(loop-over-collect (a b) (1 1) (9 9) (list a b))
(greatest 
 (group (flatten (loop-over-collect (a b) (1 1) (9 9) (list a b))) 2)
 :test #'(lambda (x y) (> (car x) (car y))))
				  

;(defmacro setup-harder-ai (this-ship enemy-ship time-tolerance to-hit-quota)
;  "this one is capable of predicting probabilities"
;  `(let ((turns-since-peek 0)
;	 (last-enemy-location (get-location ,enemy-ship))
;	 (last-enemy-direction (get-past-location ,enemy-ship))
;	 (map (make-chance-map )))
;     (labels ((look-for-ship () 
;		(if (or (ship-seen ,enemy-ship)
;			(ship-peeking ,this-ship))
;		    (progn (setq turns-since-peek 0)
;			   (setq last-enemy-location (get-location ,enemy-ship))
;			   (setq last-enemy-direction (get-past-location ,enemy-ship)))
;		    (incf turns-since-peek)))
;	      (choose-direction ()
;		(list (get-sign (- (car last-enemy-location) (ship-x ,this-ship)))
;		      (get-sign (- (cadr last-enemy-location) (ship-y ,this-ship)))))
;	      (choose-action ()
;		(look-for-ship)
;		(cond ((> (chance-to-be-here (get-location ,this-ship) 
;					     last-enemy-location
;					     turns-since-peek) 
;			  ,to-hit-quota);attempts to calculate the likelyhood of hitting the enemy ship
;		       1)
;		      ((> turns-since-peek ,time-tolerance) 2)
;		      (t 0))))
;       (set-ai ,this-ship (choose-action) (choose-direction)))))
;
;
;(cons (caddr '((x y) (1 2 3 4) ((a b) g))))
;


;(defmacro setup-simple-ai (this-ship enemy-ship time-tolerance)
;  "this one has a memory"
;  `(let ((turns-since-peek 0)
;	 (last-enemy-location (get-location ,enemy-ship))
;	 (last-enemy-direction (get-past-location ,enemy-ship)))
;     (labels ((look-for-ship () 
;		(if (or (ship-seen ,enemy-ship)
;			(ship-peeking ,this-ship))
;		    (progn (setq turns-since-peek 0)
;			   (setq last-enemy-location (get-location ,enemy-ship))
;			   (setq last-enemy-direction (get-past-location ,enemy-ship)))
;		    (incf turns-since-peek)))
;	      (choose-direction ()
;		(list (get-sign (- (car last-enemy-location) (ship-x ,this-ship)))
;		      (get-sign (- (cadr last-enemy-location) (ship-y ,this-ship)))))
;	      (choose-action ()
;		(look-for-ship)
;		(cond ((and (equal (get-location ,this-ship) last-enemy-location) 
;			    (zerop turns-since-peek))
;		       1)
;		      ((> turns-since-peek ,time-tolerance) 2)
;		      (t 0))))
;       (set-ai ,this-ship (choose-action) (choose-direction)))))
