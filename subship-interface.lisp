
(defmacro printfun (func)
  "pretty prints the output of a function"
  `(format t "~a~%" ,func))

(defun get-selection (choices)
  (format *query-io* "~{~a~%~}" choices)
  (force-output *query-io*)
  (position (read-line *query-io*) choices :test #'equal))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-direction ()
  (read-from-string (prompt-read "n s e w nw ne sw se")))

(defun show (ships)
  "prints a map"
  (loop-over (a b) (-10 -10) (10 10) 
     (let ((y (- 0 a))
	   (x b))
       (let ((sqr (get-ship-on-square ships x y)))
	 (if (= x 9) (format t " ~a~%" sqr) 
	     (format t " ~a" sqr))))))

(defun display ()
  "prints a game display for each turn"
  (let ((ships nil))
    (if (ship-seen *submarine*) (push *submarine* ships)
	(push (copy-ship *submarine*) ships))
    (if (ship-seen *destroyer*) (push *destroyer* ships))
    (if (ship-peeking *submarine*) (push *destroyer* ships))
    (show ships)
    (mapcar #'(lambda (x) (format t "(~a: x: ~a y: ~a)~%"
				  (ship-kind x) (ship-x x) (ship-y x)))
	    ships)))
