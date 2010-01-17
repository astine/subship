;subship game

;;utility macros/functions
(defmacro aif (test true-case &body false-case)
  "Anaphoric If"
  `(let ((it ,test))
     (if it ,true-case ,@false-case)))

(defmacro aand (&body forms)
  "Anaphoric And"
  (if (> (list-length forms) 1)
      `(let ((it ,(first forms)))
	 (and it (aand ,@(rest forms))))
      (car forms)))

(defun make-keyword (string)
  "Converts string into keyword"
  (intern (string-upcase string) :keyword))

(defun random-element (list)
  "Selects a random element from a list"
  (nth (random (list-length list)) list))

(defmacro setf/incf (place delta)
  "Increment place by delta. If place is nil, treat it as zero"
  `(if ,place
       (incf ,place ,delta)
       (setf ,place ,delta)))

(defmacro defmemoized (name args &body body)
  "Defines a memoized function"
  `(let ((cache (make-hash-table :test #'equal)))
     (defun ,name ,args
       (or (gethash (list ,@args) cache)
	   (let ((output (progn ,@body)))
	     (setf (gethash (list ,@args) cache) output)
	     output)))))

(defun greater (x y &optional (test #'>))
  "Returns the greater of two alternatives as defined by test."
  (if (funcall test x y) x y))

(defun greatest (list &optional (test #'>))
  "Returns the greatest of a list of alternatives as defined by test."
  (reduce (lambda (x y) 
	    (greater x y test))
	  list))

(defun average (list)
  (/ (apply #'+ list) (list-length list)))

(defun shuffle (sequence)
  "destructively shuffles a sequence"
  (labels ((swap (seq i j)
	     (let ((temp (elt seq i)))
	       (setf (elt seq i) (elt seq j))
	       (setf (elt seq j) temp))))
    (dotimes (index (length sequence))
      (swap sequence index (+ (random (- (length sequence) index)) index)))))

(defun shuffle-list (list)
  "Non-destructively shuffles a list"
  (let ((vector (map 'vector #'identity list)))
    (shuffle vector)
    (map 'list #'identity vector)))

;;Code to deal with directions and vectors

;directions are represented with keywords
;x is left and right (west and east)
;y is up and down (north and south)
;up and right are positive, left and down are negative

(defun get-directions ()
  '(:north :south :east :west :northeast :northwest :southeast :southwest))

(defun move-location (location direction)
  "Returns the location in direction of 'direction' from 'location'"
  (case direction
    (:north (cons (car location) (1+ (cdr location))))
    (:south (cons (car location) (1- (cdr location))))
    (:east (cons (1+ (car location)) (cdr location)))
    (:west (cons (1- (car location)) (cdr location)))
    (:northeast (cons (1+ (car location)) (1+ (cdr location))))
    (:northwest (cons (1- (car location)) (1+ (cdr location))))
    (:southeast (cons (1+ (car location)) (1- (cdr location))))
    (:southwest (cons (1- (car location)) (1- (cdr location))))
    (otherwise (progn (warn "Bad Direction: ~A~%" direction)
		      location))))

(defun in-vector (start direction target)
  "Returns true if target is direction from start."
  (case direction
    (:north (and (= (car start) (car target))
		 (< (cdr start) (cdr target))))
    (:south (and (= (car start) (car target))
		 (> (cdr start) (cdr target))))
    (:east (and (< (car start) (car target))
		(= (cdr start) (cdr target))))
    (:west (and (> (car start) (car target))
		(= (cdr start) (cdr target))))
    (:northeast (and (= (- (car start) (car target))
			(- (cdr start) (cdr target)))
		     (< (car start) (car target))))
    (:northwest (and (= (- (car start) (car target))
			(- (- (cdr start) (cdr target))))
		     (> (car start) (car target))))
    (:southeast (and (= (- (car start) (car target))
			(- (- (cdr start) (cdr target))))
		     (< (car start) (car target))))
    (:southwest (and (= (- (car start) (car target))
			(- (cdr start) (cdr target)))
		     (> (car start) (car target))))
    (otherwise (progn (warn "Bad Direction ~A~%" direction)
		      nil))))

;; defining the ship class basic operations
(defclass ship ()
  ((x-coordinate :accessor x-coordinate
		 :initarg :x-coordinate
		 :documentation "The current vertical location")
   (y-coordinate :accessor y-coordinate
		 :initarg :y-coordinate
		 :documentation "The current horizontal location")
   (direction :accessor direction
	      :initarg :direction
	      :documentation "The direction; can be :north, :south, :east, :west, :northeast, :northwest, :southeast, :southwest.")
   (alive :accessor alive
	  :initarg :alive
	  :initform t
	  :documentation "boolean signifier of whether ship is still in game")))

(defgeneric location (ship))

(defmethod location ((ship ship))
  "Returns the location of a ship as pair"
  (cons (x-coordinate ship) (y-coordinate ship)))

(defmethod location ((ship cons))
  ship)

(defun alive? (ship)
  (when (alive ship) ship))

;; defining the remembered-ship class and operations
(defclass remembered-ship (ship)
  ((age :accessor age
	:initarg :age
	:initform 0
	:documentation "Turns since last seen")
   (original :accessor original
	     :initarg :original
	     :documentation "Reference to the ship being remembered")))

(defmethod name ((ship remembered-ship))
  (name (original ship)))

(defun create-remembered-ship (ship)
  "Creates a remembered-ship from a ship"
  (make-instance 'remembered-ship 
		 :x-coordinate (x-coordinate ship)
		 :y-coordinate (y-coordinate ship)
		 :direction (direction ship)
		 :alive (alive ship)
		 :age 0
		 :original ship))

(defun refresh-remembered-ship (ship)
  "Refresh a remembered-ship"
  (setf (x-coordinate ship) (x-coordinate (original ship)))
  (setf (y-coordinate ship) (y-coordinate (original ship)))
  (setf (direction ship) (direction (original ship)))
  (setf (alive ship) (alive (original ship)))
  (setf (age ship) 0))

(defun age-remembered-ship (ship)
  "Age the memory of a remembered-ship by a turn"
  (incf (age ship)))

(defclass ship-with-memory (ship)
  ((name :accessor name
	 :initarg :name
	 :initform "no name"
	 :documentation "Identitfying name for ship")
   (memory :accessor memory
	   :initarg :memory
	   :initform nil 
	   :documentation "Memory of recent views of ships.")))

(defun create-ship (name location direction &optional type)
  "Returns a brand new ship object"
  (make-instance type
		 :name name
		 :x-coordinate (car location)
		 :y-coordinate (cdr location)
		 :direction direction))

(defun remember-ship (remembering-ship remembered-ship)
  (setf (getf (memory remembering-ship) (name remembered-ship))
	(create-remembered-ship remembered-ship)))

(defun refresh-memory-by-name (remembering-ship name)
  (aif (getf (memory remembering-ship) name)
       (refresh-remembered-ship it)
       (warn "Bad Name: ~A~%" name)))

(defun map-ship-memory (remembering-ship function)
  (loop for ship in (rest (memory remembering-ship)) by #'cddr
     collect (funcall function ship)))

(defun refresh-memory (remembering-ship)
  (map-ship-memory remembering-ship #'refresh-remembered-ship))
	 
(defun age-memory (remembering-ship)
  (map-ship-memory remembering-ship #'age-remembered-ship))

;; Managing ships for the game
(defvar *all-ships* nil "List for all playing ships.")

(defun add-ship (name location direction &optional type)
  "Adds a ship to the game"
  (let ((new-ship (create-ship name location direction type)))
    (dolist (ship *all-ships*)
      (remember-ship ship new-ship)
      (remember-ship new-ship ship))
    (push new-ship *all-ships*)))

(defun get-ship-by-name (name)
  "Returns ship by name"
  (dolist (ship *all-ships*)
    (when (equal (name ship) name)
      (return-from get-ship-by-name ship))))

(defun move (ship direction)
  (let ((new-location (move-location (location ship) direction)))
    (setf (x-coordinate ship) (car new-location))
    (setf (y-coordinate ship) (cdr new-location))
    (setf (direction ship) direction)))

(defun peek (peeking-ship)
  (refresh-memory peeking-ship)
  (dolist (ship (remove peeking-ship *all-ships*))
    (refresh-memory-by-name ship (name peeking-ship))))

(defun attack (attacking-ship)
  (dolist (target (remove attacking-ship *all-ships*))
    (if (in-vector (location attacking-ship) (direction attacking-ship) (location target))
	(progn (setf (alive target) nil)
	       (dolist (ship (remove target *all-ships*))
		 (refresh-memory-by-name ship (name target))))
	(refresh-memory-by-name target (name attacking-ship)))))

(defgeneric print-world (ship version))
(defmethod print-world (ship (version (eql :simple)))
  (let ((ships (make-hash-table :test #'equal)))
    (setf (gethash (cons (+ 15 (x-coordinate ship))
			 (- 15 (y-coordinate ship)))
		   ships)
	  ship)
    (loop for ship in (rest (memory ship)) by #'cddr
       do (setf (gethash (cons (+ 15 (x-coordinate ship))
			       (- 15 (y-coordinate ship)))
			 ships)
		ship))
    (dotimes (y 30)
      (dotimes (x 30)
	(aif (gethash (cons x y) ships)
	     (cond ((eql ship it)
		    (princ "@"))
		   ((not (alive (original it)))
		    (princ "X"))
		   (t
		    (case (age it)
		      (0 (princ "S"))
		      (1 (princ "s"))
		      (2 (princ "^"))
		      (otherwise (princ "~")))))
	     (princ "~")))
      (princ (string #\newline)))))

(defclass human-ship (ship-with-memory) ())
(defclass computer-ship (ship-with-memory) ())

(defgeneric turn (ship)
  (:documentation "Governs the behavior of ship"))

(defmethod turn ((ship human-ship))
  (print-world ship :simple)
  (format t "Action:~% Move)~% Peek)~% Attack)~% Quit)~%-: ")
  (case (make-keyword (read-line))
    (:move (format t "Direction: ")
	   (move ship (make-keyword (read-line))))
    (:peek (peek ship))
    (:attack (attack ship))
    (:quit (return-from turn nil)))
  (age-memory ship)
  t)

(defmemoized get-location-probability-map (location time)
  (let ((old-location-map (if (consp location)
			      (let ((map (make-hash-table :test #'equal)))
				(setf (gethash location map) 1)
				map)
			      location)))
    (if (zerop time)
	old-location-map
	(let ((new-location-map (make-hash-table :test #'equal)))
	  (maphash (lambda (location probability)
		     (dolist (direction (get-directions))
		       (setf/incf (gethash (move-location location direction) new-location-map)
				  (/ probability 8))))
		   old-location-map)
	  (get-location-probability-map new-location-map (1- time))))))

(defun chance-to-be-in-vector (start direction location-maps)
  (let ((chance 0))
    (dolist (map location-maps)
      (let ((chance-for-map 0))
	(maphash (lambda (location probability)
		   (if (in-vector start direction location)
		       (incf chance-for-map probability)))
		 map)
	    (incf chance (* chance-for-map (- 1 chance)))))
    chance))

(defun chance-to-hit (ship location-maps)
  (chance-to-be-in-vector (location ship) (direction ship) location-maps))

(defun chance-to-be-hit (ship location-maps)
  (/
   (apply #'+ (mapcar (lambda (direction)
			(chance-to-be-in-vector (location ship) direction location-maps))
		      (get-directions)))
   8))
  
; peek - memory age threshold
; attack - chance-to-hit threshold
; move - chance-to-be-hit threshold

(let ((age-threshold 2)
      (cth-threshold 1/2))

(defmethod turn ((ship computer-ship))
  (let ((maps (map-ship-memory ship (lambda (ship) 
				      (get-location-probability-map (location ship) (age ship))))))
    (cond ((> (average (map-ship-memory ship #'age)) 1)
	   (peek ship))
	  ((> (chance-to-hit ship maps) 1/2)
	   (attack ship))
	  (t
	   (move ship (greatest (shuffle-list (get-directions))
				(lambda (x y)
				  (< (chance-to-be-hit (move-location (location ship) x) maps)
				     (chance-to-be-hit (move-location (location ship) y) maps)))))))
    (age-memory ship)
    t))
	    
)

(defun main ()
  (add-ship "player" '(1 . 1) :south 'human-ship)
  (add-ship "computer" '(-1 . -1) :north 'computer-ship)
  (loop while (aand (alive? (first *all-ships*))
		    (turn it)
		    (alive? (second *all-ships*))
		    (turn it)))
  (print-world (get-ship-by-name "player") :simple)
  (setf *all-ships* nil))
