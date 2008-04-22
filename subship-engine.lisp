;(defstruct ship kind x y a b seen peeking move peek attack ai)
(defclass ship () 
  ((kind :accessor ship-kind
	 :initform 'generic-ship
	 :initarg :kind)
   (x :accessor ship-x
      :initform 0
      :initarg :x)
   (y :accessor ship-y
      :initform 0
      :initarg :y)
   (a :accessor ship-a
      :initform 0
      :initarg :a)
   (b :accessor ship-b
      :initform 0
      :initarg :b)
   (seen :accessor ship-seen
	 :initform t 
	 :initarg :seen)
   (peeking :accessor ship-peeking
	    :initform t
	    :initarg :peeking)
   (ai :accessor ship-ai
       :initform nil 
       :initarg :ai)
   (attack-function :accessor attack-function
		    :initform nil
		    :initarg :attack-function)))

(defmethod copy-ship ((ship ship))
  (make-instance 'ship
		 :kind (ship-kind ship)
		 :x (ship-x ship)
		 :y (ship-y ship)
		 :a (ship-a ship)
		 :b (ship-b ship)
		 :seen (ship-seen ship)
		 :peeking (ship-peeking ship)
		 :ai (ship-ai ship)
		 :attack-function (attack-function ship)))

(defmethod get-location ((ship ship))
  "returns a list pair of the x and y of 'ship'"
  (list (ship-x ship) (ship-y ship)))

(defmethod get-past-location ((ship ship))
  "returns a list pair of the a and b of 'ship'"
  (list (ship-a ship) (ship-b ship)))

(let ((ship-char (make-hash-table)))
  (setf (gethash 'submarine ship-char) #\s)
  (setf (gethash 'destroyer ship-char) #\d)
  (defun get-ship-char (char)
    "recieves the name of a ship and returns a character for printing"
    (gethash char ship-char)))

(let ((dir (make-hash-table)))
  (setf (gethash 'n dir) '(0 1))
  (setf (gethash 's dir) '(0 -1))
  (setf (gethash 'e dir) '(1 0))
  (setf (gethash 'w dir) '(-1 0))
  (setf (gethash 'nw dir) '(-1 1))
  (setf (gethash 'ne dir) '(1 1))
  (setf (gethash 'sw dir) '(-1 -1))
  (setf (gethash 'se dir) '(1 -1))
  (defun get-direction (direction)
    "returns a vector based on a directional symbol"
    (gethash direction dir)))

(defmethod move-ship ((ship ship) locus)
  "adds modifier 'locus' to the location (x y) of the ship"
  (let ((ox (ship-x ship))
	(oy (ship-y ship)))
    (progn
      (setf (ship-x ship) (+ ox (first locus)))
      (setf (ship-y ship) (+ oy (second locus)))
      (setf (ship-a ship) ox)
      (setf (ship-b ship) oy)
      (identity ship))))

(defmethod match-coord ((ship ship) x y)
  "returns true if 'x' and 'y' are equal to the ship's x and y"
  (and (= (ship-x ship) x)
       (= (ship-y ship) y)))

(defun get-ship-on-square (ships x y)
  "returns a the character to represent the coordinate 'x' and 'y'"
  (let ((ship (car ships)))
    (cond ((endp ships) #\~)
	  ((match-coord ship x y) (get-ship-char (ship-kind ship)))
	  (t (get-ship-on-square (cdr ships) x y)))))

;(defun distance (locus1 locus2)
;  "returns the distance between two points"
;  (sqrt (+ (a* (- (car locus1) (car locus2)) it)
;	   (a* (- (cadr locus1) (cadr locus2)) it))))

(defun distance (locus1 locus2)
  "returns the distance between two points"
  (greater (abs (- (car locus1) (car locus2)))
	   (abs (- (cadr locus1) (cadr locus2)))))

(defun depthcharge (ships)
  "returns true if the ships have the same location"
  (and (= (ship-x (car ships)) 
	  (ship-x (cadr ships)))
       (= (ship-y (car ships))
	  (ship-y (cadr ships)))))

(defun torpedo (ships)
  "returns true if the second ship is in the first's line of fire"
  (let ((ship1 (first ships))
	(ship2 (second ships)))
    (let ((xdist (- (ship-x ship2) (ship-x ship1)))
	  (ydist (- (ship-y ship2) (ship-y ship1)))
	  (xdir (- (ship-x ship1) (ship-a ship1)))
	  (ydir (- (ship-y ship1) (ship-b ship1))))
      (cond ((and (zerop xdist) (zerop ydist)) nil)
	    ((zerop ydist) (= xdir (/ xdist (abs xdist))))
	    ((zerop xdist) (= ydir (/ ydist (abs ydist))))
	    (t (and (= xdir (/ xdist (abs xdist)))
		    (= ydir (/ ydist (abs ydist)))))))))

(defclass submarine (ship) 
  ((attack-function :accessor attack-function
		    :initform #'torpedo
		    :initarg :attack-function)))

(defclass destroyer (ship) 
  ((attack-function :accessor attack-function
		    :initform #'depthcharge
		    :initarg :attack-function)))

(defmethod attack ((attacker ship) defenders)
  (setf (ship-seen attacker) t)
  (setf (ship-peeking attacker) nil)
  (funcall (attack-function attacker) (cons attacker defenders)))

(defmethod move ((ship ship) direction)
  (move-ship ship direction)
  (setf (ship-seen ship) nil)
  (setf (ship-peeking ship) nil))
  
(defmethod peek ((ship ship))
  (setf (ship-seen ship) t)
  (setf (ship-peeking ship) t))


