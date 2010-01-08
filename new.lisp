;subship game


;;utility macros
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

;;Code to deal with directions and vectors

;directions are represented with keywords
;x is left and right (west and east)
;y is up and down (north and south)
;up and right are positive, left and down are negative

(defconstant +directions+ (let ((table (make-hash-table :size 8 :rehash-threshold 0)))
			    (setf (gethash :north table) '(0 . 1))
			    (setf (gethash :south table) '(0 . -1))
			    (setf (gethash :east table) '(1 . 0))
			    (setf (gethash :west table) '(-1 . 0))
			    (setf (gethash :northeast table) '(1 . 1))
			    (setf (gethash :northwest table) '(-1 . 1))
			    (setf (gethash :southeast table) '(1 . -1))
			    (setf (gethash :southwest table) '(-1 . -1))
			    table)
  "directions represented as vectors, listed under their respective keywords")

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
		     (> (car start) (car target))))))

;;ship definitions
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
   (memory :accessor memory
	   :initarg :memory
	   :initform nil
	   :documentation "Memory of recent views of ships.")))

(defun create-ship (location direction)
  "Returns a brand new ship object"
  (unless (gethash direction +directions+)
    (error "bad direction: ~A~%" direction))
  (make-instance 'ship 
		 :x-coordinate (car location)
		 :y-coordinate (cdr location)
		 :direction direction))

(defun location (ship)
  (cons (x-coordinate ship) (y-coordinate ship)))

(defclass memory ()
  ((ships :accessor ships
	  :initarg :ships
	  :initform (make-hash-table :test #'equal)
	  :documentation "Memories of ships")
   (ages :accessor ages
	 :initarg :ages
	 :initform (make-hash-table :test #'equal)
	 :documentation "Age of each memory.")))

(defun create-memory (initial-ship-states)
  (let ((memory (make-instance 'memory)))
    (maphash (lambda (name ship)
	       (setf (gethash name (ships memory)) 
		     (create-ship (location ship) (direction ship)))
	       (setf (gethash name (ages memory)) 0))
	     initial-ship-states)
    memory))

(defgeneric forget-ship (name forgetter))

(defmethod forget-ship (name (forgetter memory))
  (remhash name (ships forgetter))
  (remhash name (ages forgetter)))

(defmethod forget-ship (name (forgetter ship))
  (forget-ship name (memory forgetter)))

(defun initialize-memories (initial-ship-states)
  (maphash (lambda (name ship)
	     (setf (memory ship) (create-memory initial-ship-states))
	     (forget-ship name ship))
	   initial-ship-states))

(defun age (memory)
  (maphash (lambda (name age)
	     (incf (gethash name (ages memory))))
	   (ages memory)))

(defvar *all-ships* (make-hash-table :test #'equal)
  "Global hash of all ships.")

(defun add-ship (name location direction)
  "Adds a ship to the global hash"
  (when (gethash name *all-ships*)
    (error "Ship ~A, already exists" name))
  (setf (gethash name *all-ships*) 
	(create-ship location direction)))

(defun getship (name)
  (gethash name *all-ships*))

(defun remove-ship (name)
  (let ((removable-ship (gethash name *all-ships*)))
    (remhash name *all-ships*)
    (maphash (lambda (ship-name ship)
	       (forget-ship ship-name removable-ship))
	     *all-ships*)))


(defgeneric remember (name ship rememberer))

(defmethod remember (name (ship ship) (rememberer memory))
  (if ship
      (progn
	(setf (gethash name (ships rememberer))
	      (create-ship (location ship) (direction ship)))
	(setf (gethash name (ages rememberer)) 0))
      (remhash name (ships rememberer))))

(defmethod remember (name ship (rememberer ship))
  (remember name ship (memory rememberer)))

(defgeneric move (ship target))

(defmethod move ((ship ship) (target list))
  (incf (x-coordinate ship) (car target))
  (incf (y-coordinate ship) (cdr target)))

(defmethod move ((ship ship) (target symbol))
  (aif (gethash target +directions+)
      (progn (setf (direction ship) target)
	     (move ship it))
    (error "bad direction: ~A~%" target)))

(defmethod move ((ship string) target)
  (aif (getship ship) 
      (move it target)
    (error "Ship ~A, doesn't exists" ship)))

(defun get-ship-name (ship)
  (block name-search
    (maphash (lambda (key value)
	       (when (eql value ship)
		 (return-from name-search key)))
	     *all-ships*)))

(defgeneric peek (peeker))

(defmethod peek ((peeker string))
  (let ((ship-peeker (getship peeker)))
    (maphash (lambda (name ship)
	       (unless (equal name peeker)
		 (remember name ship-peeker)
		 (remember peeker ship)))
	   *all-ships*)))

(defmethod peek ((peeker ship))
  (peek (get-ship-name peeker)))

(defgeneric attack (attacker))

(defmethod attack ((attacker ship))
  (maphash (lambda (name ship)
	     (when (in-vector (location attacker) (direction attacker) (location ship))
	       (remove-ship name)))
	   *all-ships*))

(defgeneric print-world (ship version))
(defmethod print-world (ship (version (eql :simple)))
  (let ((ships (make-hash-table :test #'equal)))
    (setf (gethash (cons (+ 15 (x-coordinate ship))
			 (- 15 (y-coordinate ship)))
		   ships)
	  (cons "@" 0))
    (maphash (lambda (name lship)
	       (setf (gethash (cons (+ 15 (x-coordinate lship))
				    (- 15 (y-coordinate lship)))
			      ships)
		     (cons name (gethash name (ages (memory ship))))))
	     (ships (memory ship)))
    (dotimes (y 30)
      (dotimes (x 30)
	(aif (gethash (cons x y) ships)
	    (case (cdr it)
	      (0 (princ (string-upcase (subseq (car it) 0 1))))
	      (1 (princ (string-downcase (subseq (car it) 0 1))))
	      (otherwise (princ "~")))
	  (princ "~")))
      (princ (string #\newline)))))

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))

(defmethod turn (ship)
  (print-world ship :simple)
  (format t "Action:~% Move)~% Peek)~% Attack)~% Quit)~%-: ")
  (case (make-keyword (read-line))
    (:move (format t "Direction: ")
	   (move ship (make-keyword (read-line))))
    (:peek (peek ship))
    (:attack (attack ship))
    (:quit (return-from turn nil)))
  (age (memory ship))
  t)

(defun main ()
  (add-ship "first" '(1 . 1) :south)
  (add-ship "second" '(-1 . -1) :north)
  (initialize-memories *all-ships*)
  (loop while (aand (getship "first")
		    (turn it)
		    (getship "second")
		    (turn it)))
  (clrhash *all-ships*))
