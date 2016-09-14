; (print (quote (+ 1 2)))
(print '(+ 1 2))

(print (list '(+ 2 1) (+ 3 4)))



(defun my-member (obj lst)
	(if (null lst)
		nil
		(if (eql (car lst) obj)
			(car lst)
			(my-member obj (cdr lst)))))

(print (my-member 'b '(a b c)))


;(defun askem (string)
;	(format t "~A:~%" string)
;	(read))

;(print (askem "Please enter"))

#|
(defun foo (foo)
(if foo
	(+ 1 (foo (rest foo)))
	0))
(print (foo '(1 2 3 4 5 6 7 8 0 9 8 7 6 5)))
|#


C-x C-s - save
C-x b - change buffer
C-c M-k - compile
