; 1a
; > In a few sentences, discuss other ways of how we could have defined the notion of context here.
; We could have used context windows - plus-minus neighborhood (i.e. consider only the words that are in n-window around the given word).
; We could also use weighted context windows - give higher score to the words that are closer to the given word and lower - to those that are on the longer distance.
; Another option is to use bag of words on the document level - if we had much more than one document and the document size would be smaller.

; 2a
; define a structure vs that has at least the following slots: matrix, similarity-fn
(defstruct vs
	matrix ; hashmap of hashmaps, see (*) below
	similarity-fn
	words) ; list of words from words.txt

(defparameter vspace (make-vs))

; (*)
; JSON-representation of vs.matrix (hashmap of hashmaps or in JSON terms - object of objects):
; {
;	"university":
;		{
; 			"school": 2,
;			"classes": 3,
;			"milk": 1,
;			...
;		},
;	"america":...
; }

; (print vspace)

; 2b
; define a function read-corpus-to-vs

; Helper function to remove whitespaces, tabs, comma and dot from the word token
(defun normalize-token (word)
	(string-downcase (string-trim '(#\Space #\Tab #\, #\.) word)))

; Test normalize-token
;(print (normalize-token ".TesT, "))


; from exercise 1
(defun tokenize (string)
	(loop
	for start = 0 then (+ space 1)
	for space = (position #\space string :start start)
	for token = (normalize-token (subseq string start space))
	unless (string= token "") collect token
	until (not space)))


; stop-list (from exercise file)
(defparameter *stop-list*
	'("a" "about" "also" "an" "and" "any" "are" "as" "at" "be" "been"
	"but" "by" "can" "could" "do" "for" "from" "had" "has" "have"
	"he" "her" "him" "his" "how" "i" "if" "in" "is" "it" "its" "la"
	"may" "most" "new" "no" "not" "of" "on" "or" "she" "some" "such"
	"than" "that" "the" "their" "them" "there" "these" "they" "this"
	"those" "to" "was" "we" "were" "what" "when" "where" "which"
	"who" "will" "with" "would" "you"))
; (print *stop-list*)

; Function that loads all words from words.txt to vs.words list
(defun read-words-to-list (word-file)
	(let ((words NIL))
		(with-open-file (stream word-file :direction :input)
			(loop
				for word = (read-line stream nil)
				while word do
				(if (not (member (normalize-token word) words :test #'equal))
					(setq words (cons (normalize-token word) words)))
			)
		)
	words))

; Parse a new line of corpus file:
; for every token of a line check whether it is in vs.words
; if yes - loop through all tokens of a line and add to a corresponding hashmap
(defun parse-line (line vspace)
	(loop
		for token in (tokenize line) ; iterate words of a sentence to find a token from words.txt
		for token-idx from 0 ; pick token index
		do (if (member (normalize-token token) (vs-words vspace) :test #'equal)
			(let ((ntoken (normalize-token token))) ; ntoken = normalized token
				(if (not (gethash ntoken (vs-matrix vspace))) ; create a new feature vector for ntoken if not exists
					(setf (gethash ntoken (vs-matrix vspace)) (make-hash-table :test 'equal))
					)
				(loop
					for word in (tokenize line) ; iterate words of a sentence
					for word-idx from 0 ; pick word index
					do (if (and
						(not (eql token-idx word-idx)) ; if not the same word as token
						(> (length (normalize-token word)) 1) ; and word length > 1 (to skip rubbish)
						(not (member (normalize-token word) *stop-list* :test #'equal))) ; and not in stop list
							; do
							(let ((nword (normalize-token word)) (hmap (gethash ntoken (vs-matrix vspace))))
								(if (gethash nword hmap) ; if the word exists in feature vector (hashmap)
									(setf (gethash nword hmap) (+ 1 (gethash nword hmap))) ; then increment
									(setf (gethash nword hmap) 1) ; else save as 1 - first time
									)
								)
					)
				)
			)
		)
	))

(defun read-corpus-to-vs (corpus words)
	(let ((vspace (make-vs)))
		(setf (vs-words vspace) (read-words-to-list words))
		(setf (vs-matrix vspace) (make-hash-table :test 'equal))
		(with-open-file (stream corpus :direction :input)
			(loop
				for line = (read-line stream nil)
				while line
				do (parse-line line vspace)
			)
		)
		vspace))

(defparameter space (read-corpus-to-vs "brown2.txt" "words.txt"))
;(print space)

; 2c
(defun get-feature-vector (space word)
	(gethash (normalize-token word) (vs-matrix space))
	)
;(print (get-feature-vector space "university"))

; 2d
; convert the hashmap (in our case - feature vector) into a list of (key value) entries
(defun hash-map-to-list (hmap)
	(let ((alist nil))
		(maphash (lambda (k v)
			(push (cons k v) alist))
			hmap)
	alist))

; return subsequence of an entry list with (key value)-entries ordered by value (cdr)
; Ref. to stackoverflow: http://stackoverflow.com/questions/7508450/whats-the-best-way-to-sort-a-hashtable-by-value
(defun hash-table-top-n-values (table n)
	(subseq (sort (hash-map-to-list table) #'> :key #'cdr) 0 n))

(defun print-features (space word k)
	(let ((hmap (get-feature-vector space word)))
		(format t "Print ~a top features for ~a:~%" k word)
		(loop
			for entry in (hash-table-top-n-values hmap k)
			do (format t "~a ~a~%" (car entry) (cdr entry))
		)
	))
(print-features space "university" 12)

; 3a
; euclidean-length
(defun euclidean-length (hmap)
	(let ((len 0))
		(maphash (lambda (k v)
			(incf len (* v v)))
			hmap)
	(sqrt len)))
;(print (euclidean-length (get-feature-vector space "university")))

; 3b
(defun length-normalize-vs (space)
	(let ((matrix (vs-matrix space)))
		(loop
			for features being each hash-value of matrix ; for each feature vector
			do (let ((norm (euclidean-length features))) ; calculate euclidean norm
				(loop
					for token being each hash-key of features ; iterate through all elements of feature vector
					for value being each hash-value of features
					do (setf (gethash token features) (/ value norm)) ; and update the value to value/norm
				)
			)
		)
	))

(format t "Before normalization: ~a~%" (euclidean-length (get-feature-vector space "university")))
(length-normalize-vs space)
(format t "After normalization: ~a~%" (euclidean-length (get-feature-vector space "university")))


; 3c
(defun dot-product (x y)
	(let ((result 0))
		(if (and x y)
			(loop
				for xkey being each hash-key of x
				for xvalue being each hash-value of x
				do (if (gethash xkey y) ; if the same feature is present in y
						(incf result (* xvalue (gethash xkey y))) ; add product of x[feature] and y[feature] to result
				))
			)
	result))

;(print (dot-product (get-feature-vector space "russia") (get-feature-vector space "moscow"))) ; 0.1730942

(setf (vs-similarity-fn space) (lambda (x y) (dot-product x y)))
;(print (vs-similarity-fn space))

; 3d
(defun word-similarity (space word1 word2)
	(funcall (vs-similarity-fn space) (get-feature-vector space word1) (get-feature-vector space word2))
	)
(print (word-similarity space "university" "college"))
(print (word-similarity space "university" "rice"))
