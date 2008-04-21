
; (defmacro once-only ((&rest names) &body body)
;   (let ((gensyms (loop for n in names collect (gensym))))
;    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
;        `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
;           ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
;              ,@body)))))


(defun pop-out (lst n)
  "returns the 'n'th item of 'lst' and a list of al the elements of 'lst' save the 'n'th"
  (values (nth n lst)
	  (append (butlast lst (- (list-length lst) n))
		  (nthcdr (1+ n) lst))))

(defun shuffle (lst)
  "returns a list which is a random ordering of the elements in 'lst'"
  (labels ((rec (in out)
		(if (null in) out
		  (multiple-value-bind (item rst) (pop-out in (random (list-length in)))
		    (rec rst (cons item out))))))
    (rec lst nil)))

(defun cond-match-row (test-type match return-value)
  "generates a single row for cond-match"
  `((equal ,test-type ,match) ,return-value))

(defmacro cond-match (test-type num-terms matches return-values &rest default)
  " a method of implementing case statements with lists"
  `(cond ,@(mapcar #'cond-match-row 
		   (make-list num-terms :initial-element test-type) 
		   (if (atom matches) (funcall matches)
		     matches)
		   (if (atom return-values) (funcall return-values)
		     return-values))
	 (t ,@default)))

(defmacro intersperse (&rest lists)
  "returns a list consisting of the elements of two other lists interpersed"
  `(mapcan #'list ,@lists))

(defun genums (start end &optional (inc 1))
  "returns a list of all integers bounded by 'start' and 'end' begining with start
  and separated 'inc'"
  (labels ((rec (start end inc sum fn)
		(if (funcall fn start end) sum
		  (rec (+ start inc) end inc (cons start sum) fn))))
    (nreverse (rec start end inc nil (if (plusp inc) #'> #'<)))))

(defun longer (lst1 lst2)
  "returns true if 'lst1' is longer than 'lst2'"
  (labels ((rec (x1 x2)
		(cond
		  ((null x1) nil)
		  ((null x2) t)
		  (t (rec (cdr x1) (cdr x2))))))
    (if (and (listp lst1) (listp lst2)) (rec lst1 lst2)
      (> (length lst1) (length lst2)))))

(defun group (lst n)
  "returns a list of sublists each consisting of 'n' elemenst of 'lst'"
  (if (zerop n) (error "zero length"))
  (labels ((rec (x acc)
		(let ((rst (nthcdr n x)))
		  (if (consp rst)
		    (rec rst (cons (subseq x 0 n) acc))
		    (nreverse (cons x acc))))))
    (if lst (rec lst nil) nil)))

(defun assoc-replace (alst item &key (key nil) (test #'equal) value)
  "returns an alist that is a copy of 'alst' except that the cdr of each cons whose car satisfies
  the test provided is replaced with 'item'"
  (mapcar #'(lambda (x) (if (if key (funcall key (car x))
			      (funcall test (car x) value)) (cons (car x) item)
			  x))
	  alst))

(defun filter (fn lst)
  "removes all elements of 'lst' that satisfy 'fn'"
  (let ((acc nil))
    (dolist (x lst)
      (if (not (funcall fn x)) (push x acc)))
    (nreverse acc)))

(defun flatten (tree)
  "returns a list consisting of all elements of 'tree'"
  (labels ((rec (x acc)
		(cond
		  ((null x) acc)
		  ((atom x) (cons x acc))
		  (t (rec (car x) (rec (cdr x) acc))))))
    (rec tree nil)))

(defun prune (fn tree)
  "removes all elements of 'tree' that satisfy 'fn'"
  (labels ((rec (x acc)
		(cond
		  ((null x) (nreverse acc))
		  ((consp (car x))
		   (rec (cdr x) 
			(cons (rec (car x) nil) acc)))
		  (t (rec (cdr x)
			  (if (funcall fn (car x)) acc
			    (cons (car x) acc)))))))
    (rec tree nil)))

(defun before (a b lst &key (test #'eql))
  "returns true if 'a' is found before 'b' in 'lst',
  does not require that 'b' be actually in the 'lst'"
  (and lst
       (let ((x (car lst)))
	 (cond ((funcall test b x) nil)
	       ((funcall test a x) lst)
	       (t (before a b (cdr lst) :test test))))))

(defun after (a b lst &key (test #'eql))
  "returns true if 'a' is found after 'b' in 'lst'"
  (let ((wlft (before b a lst :test test)))
    (member a wlft :test test)))

(defun map-> (fn start test-fn inc-fn)
  "returns a list of objects returned by calls of 'fn' on a series of objects generated by 
  'test-fn,' 'inc-fn,' and 'start'"
  (do ((i start (funcall inc-fn i))
       (result nil (cons (funcall fn i) result)))
    ((funcall test-fn i) (nreverse result))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun def-adder (name n) 
  `(defun ,name (x) (+ x ,n)))

(defmacro define-adder (name n)
  (def-adder name n))

(defmacro with-gensyms (symbs &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 symbs)
     ,@body))

(defmacro for (var start end inc &body body)
  "implements a for loop ex:
  (for x 1 10 (print x))"
  (let ((stop (gensym)))
    `(do ((,var ,start (funcall ,inc ,var))
	  (,stop ,end))
       ((> ,var ,stop)) ,@body)))

(defmacro while (test &body body)
  "implements a simple while loop"
  `(do () ((not ,test)) ,@body))

(defmacro defstruct-from-func (name paramlist)
  `(defstruct ,name ,@(funcall paramlist)))

(defun greater (x y &key (test #'>))
  (if (funcall test x y) x
    y))

(defun greatest (vals &key (test #'>))
  "returns the largest element of a list"
  (labels ((rec (lst)
		(if (= (list-length lst) 1) (car lst)
		  (if (funcall test (car lst) (cadr lst)) (rec (cons (car lst) (cddr lst)))
		    (rec (cons (cadr lst) (cddr lst)))))))
    (rec vals)))

(defmacro non-destructive-set-for-struct (struct-type &rest elements)
  "creates a function which returns a copy of a structure with the option to use keywords
  to explicity specify separate values for the slots"
  `(defun ,(reread "set-" struct-type) (struct &key ,@elements)
     (,(reread "make-" struct-type) ,@(mapcan #'(lambda (elem) 
						  `(,(reread #\: elem) 
						     (if ,elem ,elem 
						       (,(reread struct-type #\- elem) struct)))) 
					      elements))))

(defun counter (alst val)
  (cond ((endp alst) (acons val 1 nil))
	((equal (caar alst) val) (acons val (1+ (cdar alst)) (cdr alst)))
	(t (cons (car alst) (counter (cdr alst) val)))))

(defun test-fn (fn n lst)
  (do ((i 0 (1+ i))
       (res (funcall fn lst) (funcall fn lst))
       (cnt nil (counter cnt res)))
    ((= i n) cnt)))

(defun rot-num (num start end)
  (if (= num end) start
    (1+ num)))

(defmacro nullify (&rest rest)
  "used to create inverse versions of functions prefixed with a '!'"
  `(progn 
     ,@(mapcar #'(lambda (func)
		   `(defun ,(reread #\! (car func)) (,@(cdr func)) (not (,@func))))
	       rest)))

(defmacro loop-over (vars mins maxs &body body)
  "creates a nested loop structure"
  (labels ((rec (vs mns mxs)
		(if (endp vs) body
		  `((do ((,(car vs) ,(car mns) (1+ ,(car vs)))) 
		      ((= ,(car vs) ,(car mxs))) ,@(rec (cdr vs) (cdr mns) (cdr mxs)))))))
    (car (rec vars mins maxs))))

(defmacro loop-over-collect (vars mins maxs &body body)
  "creates a nested loop structure"
  (labels ((rec (vs mns mxs)
		(if (endp vs) body
		  `((do ((,(car vs) ,(car mns) (1+ ,(car vs)))
			 (,(reread (car vs) "lst") nil (cons ,@(rec (cdr vs) (cdr mns) (cdr mxs)) 
							     ,(reread (car vs) "lst"))))
		      ((= ,(car vs) ,(car mxs)) ,(reread (car vs) "lst")))))))
    (car (rec vars mins maxs))))


(defun get-sign (n)
  "returns either 1 or -1 depending on the sign of 'n'"
  (cond ((minusp n) -1)
	((plusp n) 1)
	(t 0)))

(defmacro test-adjacent-squares (location var1 var2 &body body)
  "given a pair of numbers, test body on each of them and return
   a list of the results"
  `(loop-over-collect (,var1 ,var2)
      ((1- (car ,location)) (1- (cadr ,location)))
      ((+ (car ,location) 2) (+ (cadr ,location) 2))
      ,@body))
