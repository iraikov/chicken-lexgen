
(require-extension scheme (chicken base)
                   (chicken format)
                   srfi-1 srfi-14
                   test lexgen yasos yasos-collections)

(define a-pat (tok #\a (try char=?)))
(define b-pat (tok #\b (try char=?)))
(define a-then-b-pat (seq a-pat b-pat))
(define a-or-b-pat (bar a-pat b-pat))
(define a-star-pat (star a-pat))
(define a-star-or-b-pat (bar (star a-pat) b-pat))
(define a-or-b-star-pat (star a-or-b-pat))
(define a-b-opt-pat (seq a-pat (opt b-pat)))
(define b-opt-a-pat (seq (opt b-pat) a-pat))
(define a-b-opt-a-pat (seq a-pat (seq (opt b-pat) a-pat)))
(define a-star-b-opt-pat (seq (star a-pat) (opt b-pat)))
(define aabac-pat (lit "aabac"))
(define drop-aabac-pat (drop aabac-pat))
(define aa-pat (lit "aa"))
(define n4-pat (lst (list-tabulate 4 (lambda (i) (range #\0 #\9)))))

(define abc-stream       `(() ,(string->list "abc")))
(define bac-stream       `(() ,(string->list "bac")))
(define aabac-stream     `(() ,(string->list "aabac")))
(define aaaabac-stream   `(() ,(string->list "aaaabac")))
(define num-stream       `(() ,(string->list "1234")))


(define (err s)
  (print "lexical error on stream: " s)
  `(error))

(test-group "lexgen test"
	    (test (sprintf "match [a] on ~S" "abc") 
		   `((#\a) (#\b #\c)) (a-pat identity err abc-stream))

	    (test (sprintf "match [b] on ~S" "abc") 
		   `(error) (b-pat identity err abc-stream))

	    (test (sprintf "match ab on ~S" "abc") 
		   `((#\b #\a ) ( #\c))
		   (a-then-b-pat identity err abc-stream))

	    (test (sprintf "match a|b on ~S" "abc") 
		   `((#\a) (#\b #\c)) 
		   (a-or-b-pat identity err abc-stream))

	    (test (sprintf "match a|b on ~S" "bac") 
		   `((#\b) (#\a #\c))
		   (a-or-b-pat identity err bac-stream))

	    (test (sprintf "match a* on ~S" "abc") 
		   `((#\a) (#\b #\c))
		   (a-star-pat identity err abc-stream))

	    (test (sprintf "match a* on ~S" "aabac") 
		  `((#\a #\a) (#\b #\a #\c))
		  (a-star-pat identity err  aabac-stream))

	    (test (sprintf "match (a*|b) on ~S" "aabac") 
		   `((#\a #\a) (#\b #\a #\c))
		   (a-star-or-b-pat identity err  aabac-stream))

	    (test (sprintf "match (a|b)* on ~S" "abc") 
		   `((#\b #\a) (#\c))
		   (a-or-b-star-pat identity err  abc-stream))

	    (test (sprintf "match (a|b)* on ~S" "aabac") 
		   `((#\a #\b #\a #\a) (#\c))
		   (a-or-b-star-pat identity err  aabac-stream))

	    (test (sprintf "match ab? on ~S" "abc") 
		   `((#\b #\a) (#\c)) 
		   (a-b-opt-pat identity err  abc-stream))

	    (test (sprintf "match ab? on ~S" "aabac") 
		   `((#\a) (#\a #\b #\a #\c)) 
		   (a-b-opt-pat identity err  aabac-stream))

	    (test (sprintf "match b?a on ~S" "abc") 
		   `((#\a) (#\b #\c)) 
		   (b-opt-a-pat identity err  abc-stream))

	    (test (sprintf "match ab?a on ~S" "aabac") 
		   `((#\a #\a) (#\b #\a #\c)) 
		   (a-b-opt-a-pat identity err  aabac-stream))

	    (test (sprintf "match a*b? on ~S" "aabac") 
		   `((#\b #\a #\a) (#\a #\c))
		   (a-star-b-opt-pat identity err aabac-stream))

	    (test (sprintf "match literal string ~S" "aabac") 
		   `((#\c #\a #\b #\a #\a) ()) 
		   (aabac-pat identity err aabac-stream))

	    (test (sprintf "match and drop literal string ~S" "aabac") 
		   `(() ()) 
		   (drop-aabac-pat identity err aabac-stream))

	    (test (sprintf "match n4 on  ~S" "1234") 
		   `((#\4 #\3 #\2 #\1) ()) 
		   (n4-pat identity err num-stream))

	    )
;; A pattern to match floating point numbers. 
;; "-"?(([0-9]+(\\.[0-9]+)?)|(\\.[0-9]+))([eE][+-]?[0-9]+)? 

(define numpat
  (let* ((digit        (range #\0 #\9))
	 (digits       (pos digit))
	 (fraction     (seq (char #\.) digits))
	 (significand  (bar (seq digits (opt fraction)) fraction))
	 (exp          (seq (set "eE") (seq (opt (set "+-")) digits)))
	 (sign         (opt (char #\-))))
    (seq sign (seq significand (opt exp)))))

(print (lex numpat err "-123.45e-6"))

(test-group "lexgen numpat test"
	    (test (sprintf "match numpat on ~S" "-123.45e-6")
		   `(#\- #\1 #\2 #\3 #\. #\4 #\5 #\e #\- #\6)
		   (car (lex numpat err "-123.45e-6")))
	    (test (sprintf "match numpat on ~S" "hi there")
		  `(error) 
		  (lex numpat err "hi there")))

(define (->char-list s)
  (if (string? s) (string->list s) s))

(define (collect cs) 
  (let loop ((cs cs) (ax (list)))
    (cond ((null? cs)         `(,(list->string ax)))
	  ((atom? (car cs))   (loop (cdr cs) (cons (car cs) ax)))
	  (else               (cons (list->string ax) cs)))))

(define (make-exp x)
  (or (and (pair? x) 
	   (let ((x1 (collect x)))
	     (list `(exp . ,x1)))) x))

(define (make-significand x)
  (or (and (pair? x) 
	   (let ((x1 (collect x)))
	     (cons `(significand ,(car x1)) (cdr x1)))) x))

(define (make-sign x)
  (or (and (pair? x) 
	   (let ((x1 (collect x)))
	     (cons `(sign ,(car x1)) (cdr x1)))) x))

(define (check s) (lambda (s1) (if (null? s1) (err s) s1)))

(define bnumpat 
  (let* ((digit        (range #\0 #\9))
	 (digits       (star digit))
	 (fraction     (seq (char #\.) digits))
	 (significand  (bar (seq digits (opt fraction)) fraction))
	 (exp          (seq (set "eE") (seq (opt (set "+-")) digits)))
	 (sign         (opt (char #\-)) )
	 (pat          (seq (bind make-sign sign) 
			    (seq (bind make-significand significand)
				 (bind make-exp (opt exp))))))
    pat))

(define (num-parser s) (car (lex bnumpat err s)))


(test-group "lexgen num-parser test"
	    (test (sprintf "match num-parser on ~S" "-123.45e-6")
		   `((sign "-") (significand "123.45") (exp "e-6"))
		   (num-parser "-123.45e-6")) )
	    


;; Tokens with position information

       
(define-record-type postok
  (make-postok pos token)
  postok?
  (pos        postok-pos )
  (token      postok-token )
  )

(define pos? pair?)
(define pos-row car)
(define pos-col cdr)
(define make-pos cons)

(define-record-printer (postok x out)
  (fprintf out "#<token ~A: ~A>" 
	   (postok-pos x)
	   (postok-token x)))
	  
(define (getpos p)
  (let ((f (lambda (in) (and (pair? in) (postok-pos (car in)))))
	(g (lambda (i s) (list (make-postok i (car s))))))
    (rebind f g p)))

;; (define pos-<Input>
;;   (let ((pos-tail
;; 	 (lambda (strm)
;; 	   (cond ((or (null? strm) (null? (cdr strm)))  '())
;; 		 (else
;; 		  (let* ((curtok  (car strm))
;; 			 (pos0    (postok-pos curtok))
;; 			 (pos1    (let ((row0 (pos-row pos0))
;; 					(col0 (pos-col pos0)))
;; 				    (case (cadr strm)
;; 				      ((#\newline)  (make-pos (+ 1 row0) 1))
;; 				      ((#\return)   (make-pos row0 1))
;; 				      (else         (make-pos row0 (+ 1 col0))))))
;; 			 (res (cons (make-postok pos1 (cadr strm)) (cddr strm))))
;; 		    res)))))
;; 	(pos-null? null?)
;; 	(pos-head  (compose postok-token car)))
;;     (make-<Input> pos-null? pos-head pos-tail)))

(define (make-pos-stream strm)
  (let ((begpos (make-pos 1 1)))
    `(() ,(cons (make-postok begpos (car strm)) (cdr strm)))))
  
(define pos-numpat-stream
  (make-pos-stream (string->list "-123.45e-6")))

;; (define pbnumpat 
;;   (let* ((digit        (pos/range #\0 #\9))
;; 	 (digits       (star digit))
;; 	 (fraction     (seq (pos/char #\.) digits))
;; 	 (significand  (bar (seq digits (opt fraction)) fraction))
;; 	 (exp          (seq (pos/set "eE") (seq (opt (pos/set "+-")) digits)))
;; 	 (sign         (opt (pos/char #\-)) )
;; 	 (pat          (seq (getpos (bind make-sign sign))
;; 			    (seq (getpos (bind make-significand significand))
;; 				 (getpos (bind make-exp (opt exp)))))))
;;     pat))

(define (pos-num-parser s)  (car (lex pbnumpat err s)))

;; (test-group "lexgen pos-num-parser test"
;; 	    (test (sprintf "match pos-num-parser on ~S" "-123.45e-6")
;; 		   `(,(make-postok (make-pos 1 1) `(sign "-"))
;; 		     ,(make-postok (make-pos 1 2) `(significand "123.45"))
;; 		     ,(make-postok (make-pos 1 8) `(exp "e-6")))
;; 		   (pos-num-parser pos-numpat-stream))
;; 	    )

(test-exit)
