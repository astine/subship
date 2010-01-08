;;;Loading the libraries, change paths if appropriate:
(load "c:/Documents and Settings/Administrator/subship/aclib.lisp")
(load "c:/Documents and Settings/Administrator/subship/subship-engine.lisp")
(load "c:/Documents and Settings/Administrator/subship/subship-interface.lisp")
(load "c:/Documents and Settings/Administrator/subship/subship-ai.lisp")

;;;initializing the ships:
(defvar *submarine* (make-instance 'submarine 
				   :kind 'submarine
				   :x 1
				   :y 1
				   :a 0
				   :b 0))

(defvar *destroyer* (make-instance 'destroyer 
				   :kind 'destroyer
				   :x -1
				   :y -1
				   :a 0
				   :b 0))

(makunbound '*submarine*)
(makunbound '*destroyer*)

(setup-human-ai *submarine* (list *destroyer*))
(setup-harder-ai *destroyer* *submarine* 1 1 1 4 1/8)

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
