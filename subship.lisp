;;;Loading the libraries, change paths if appropriate:
(load "~/Programs/lisp/aclib.lisp")
(load "~/Programs/lisp/subship-engine.lisp")
(load "~/Programs/lisp/subship-interface.lisp")
(load "~/Programs/lisp/subship-ai.lisp")

;;;initializing the ships:
(defvar *submarine* (make-instance 'submarine 
				   :kind 'submarine
				   :x -1
				   :y 1
				   :a 0
				   :b 0))

(defvar *destroyer* (make-instance 'destroyer 
				   :kind 'destroyer
				   :x 1
				   :y -1
				   :a 0
				   :b 0))

(setup-submarine `(,*submarine* ,*destroyer*))
(setup-destroyer `(,*destroyer* ,*submarine*))
(setup-human-ai *submarine*)
(setup-harder-ai *destroyer* *submarine* 4 1/8)

;;;main game loop:
(let ((x nil))
  (while (null x)
    (display)
    (setq a (funcall (ship-ai *submarine*)))
    (case a
      ('quit (setq x t))
      ('hit (progn (format t "you win!~%")
		   (setq x t)))
      ('miss (format t "you miss!~%"))
      (t 0))
    (when (null x)
      (display)
      (prompt-read "press enter")
      (setq a (funcall (ship-ai *destroyer*)))
      (case a
	('hit (progn (format t "you lose!~%")
		     (setq x t)))
	(t 0)))))
