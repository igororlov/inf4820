;;; Task 1a
;;; Check recursively whether the element of a list is pear

(print 'Task_1a)

(defun find-pear1 (lst)
	(if (null lst)
		NIL
		(if (eql (car lst) 'pear)
			(car lst)
			(find-pear1 (cdr lst)))))

(setq l1 '(apple orange pear lemon))
(print (find-pear1 l1))



;;; Task 1b
;;; Check recursively whether the first element of the parent list element

(print 'Task_1b)

(defun find-pear2 (lst)
	(if (null lst)
		NIL
		(if (eql (car (car lst)) 'pear)
			(car (car lst))
			(find-pear2 (cdr lst)))))
	
(setq l2 '((apple orange) (pear lemon) (alex test)))
(print (find-pear2 l2))



;;; Task 1c
;;; The same as 1b

(print 'Task_1c)

(setq l3 '((apple) (orange) (pear)))
(print (find-pear2 l3))



;;; Task 1d

(print 'Task_1d)

(print (cons 'apple (cons 'orange (cons 'pear (cons 'lemon nil)))))
(print (cons (cons 'apple (cons 'orange NIL)) (cons (cons 'pear (cons 'lemon NIL)) NIL)))
(print (cons (cons 'apple NIL) (cons (cons 'orange NIL) (cons (cons 'pear NIL) NIL))))



;;; Task 1e

(print 'Task_1e)

(defparameter *foo* '(a b c d e f g h i j k l m))

; Solution 1
(print (car (last (butlast *foo*))))

; Solution 2
(defun my-next-to-last (lst)
	(if (eql (length lst) 2)
		(car lst)
		(my-next-to-last (cdr lst))))
(print (my-next-to-last *foo*))



;;; Task 2
; Foo is a function that recursively calculates the length of a list by adding 1 for each element,
; until the list is empty
; In a function body foo is used both as a function parameter (if foo and (rest foo))
; as as a function name - (foo (rest ...))

(print 'Task_2)

(defun foo (foo)
(if foo
	(+ 1 (foo (rest foo)))
	0))
(print (foo '(a b c d e)))



;;; Task 3a

(print 'Task_3a)

(print
	(let ((foo (list 0 42 2 3)))
	(pop foo) ; Just remove the first element
	(first foo))
)


;;;  Task 3b - Not perfect - returns (42) instead of 42 (use pairlis?)
;;; "rest" always returns list! how can this be 42?
(print 'Task_3b)

(print
	(let* ((keys '(:a :b :c))
		(values '(0 1 2))
		(pairs '((:a 1) (:b 42) (:c 2))))
	(rest (assoc :b pairs)))
)


;;; Task 3c
;;; foo is a new hashtable,
;;; setf changes the value of place to be newvalue. 
;;; foo['meaning'] = 41 

(print 'Task_3c)

(print
	(let ((foo (make-hash-table)))
	(setf (gethash 'meaning foo) 41)
	(setf (gethash 'meaning foo) (+ 1 (gethash 'meaning foo))) ; increment the value stored foo['meaning']
	(gethash 'meaning foo))
)



;;; Task 3d
;;; foo is a new 

(print 'Task_3d)

(print
	(let ((foo (make-array 5)))
	(setf (aref foo 2) 42) ; set foo[2]=42
	(aref foo 2))
)



;;; Task 4a - recursive count

(print 'Task_4a)

(defun count-member (symbol lst)
	(if (null lst)
		0
		(if (eql symbol (car lst))
			(+ 1 (count-member symbol (rest lst)))
			(count-member symbol (rest lst)))))
(print (count-member 'c '(c a a c a c)))


;;; Task 4b - loop-based count

(print 'Task_4b)

(defun count-member2 (symbol lst)
	(loop for i in lst count (eql i symbol)))
(print (count-member2 'c '(c a a c a c)))


;;; Task 5
;; position (position 1 (list 1 2 3 4 5 6)) -> 0
;; 

(defun tokenize (string)
	(loop
		for start = 0 then (+ space 1)
		for space = (position #\space string :start start)
		for token = (subseq string start space)
		unless (string= token "") collect token
		until (not space)))

;; 5a
;; The return value of the given expression is a list with word-like string tokens
;; - basically the input text splitted into substrings by a whitespace character.

;; 5b - tokens count: 23132
(defparameter *corpus*
	(with-open-file (stream "brown1.txt" :direction :input)
		(loop
			for line = (read-line stream nil)
			while line
			append (tokenize line))))
(print 'Task_5b)
(print (list-length *corpus*))

;; 5c
;; "What exactly is our current strategy for tokenization?"
;; You just split every line into words by spaces. Just like line.split(" ") in Java.
;; Of course, this is not the most effective strategy and can be further improved
;; for example, "think," and "think" have to be the  same token, no need to separate them
;; Character case has also to be ignored, "Once" and "once" are the same word. Need to convert to lowercase.
;; Expressions like "I've" and "we've" can be splitted into "I" and "have" or "I" and "ve".
;; And vice versa, the sequence "Rev." "T." "F." "Zimmerman" - has to be joined in a single token, having T anf F tokens makes no sence.
;; Actually, "T F Zimmerman" and "Zimmerman T F" has to be the same token as well, but this is harder to achieve :)

;; 5d

(defun maptokens (corpus)
	(let ((foo (make-hash-table :test 'equal)))
		(loop for i in corpus do (
			if (gethash i foo)
				(setf (gethash i foo) (+ 1 (gethash i foo)))
				(setf (gethash i foo) 1)))
		foo)
	)

(defparameter *tokensmap* (maptokens *corpus*))
;(print *tokensmap*)

;; 5e
; How many: 8091
(print 'Task_5e)
(print (hash-table-size *tokensmap*))

;; 5f
; HAVEN'T DONE


;; 5d

; BONUS: recursive implementation, fails due to stackoverflow
#|
(defun addtoken (lst map)
	(if (null lst)
		NIL
		(let ((i (first lst)))
			(if (gethash i map)
				(setf (gethash i map) (+ 1 (gethash i map)))
				(setf (gethash i map) 1))
			(addtoken (rest lst) map)
			map)))

(defun maptokens (corpus)
	(let ((foo (make-hash-table :test 'equal)))
		(addtoken corpus foo)
		foo))

(defparameter *tokensmap* (maptokens (list 'igor 'igor 'nastya)))
(print *tokensmap*)
(defparameter *tokensmap* (maptokens *corpus*)) ; works but stack overflow on corpus...
(print *tokensmap*)
|#