;;
;;  Lexer combinator library.
;;
;;  Based on the SML lexer generator by Thant Tessman.
;;
;;  Copyright 2009-2019 Ivan Raikov.
;;
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.

(module lexgen

  ( seq star bar 
    try pass pos opt lst  
    bind bind* rebind rebind* drop
    lex tok char range set lit
    )


  (import scheme (chicken base)
          (only srfi-1 first second filter-map fold concatenate every lset<= )
          utf8 utf8-srfi-14 srfi-127)


;;
;;   This is a lexer generator comprised in its core of five small
;;   functions. The programmer assembles these functions into regular
;;   expression pattern-matching functions.
;;
;;   The idea is that a pattern matcher function takes a list of
;;   streams, and returns a new list of streams advanced by every
;;   combination allowed by the pattern matcher function. 
;;
;;   A stream is a list that can take one of two forms: 
;;   
;;   1) A list of two elements: the first element is a list of
;;   elements consumed by the pattern matcher; the second element is a
;;   list of characters not yet consumed. E.g., the list
;;
;;   ((a) (b c d e))
;;
;;   represents a stream that contains the consumed character a,
;;   and the unconsumed characters b c d e. 
;;
;;   2) A list of three elements: the first two elements are as
;;   before, but the third element is a procedure that is applied to
;;   the tail of the unconsumed list, in order to obtain the next
;;   character. E.g., the list:
;;
;;   ((a) (b <port>) <procedure (lambda (in) (list (read-char in) in))>
;;
;;   represents a stream that contains the consumed character a, the
;;   unconsumed character b, and an input port to read subsequent
;;   character from; and a procedure that reads one character from the
;;   input port, and returns it along with the modified port. Note
;;   that the use of side-effecting structures such as ports will lead
;;   to erroneous results with backtracking parsers.
;;
;;   Also note that the number of streams returned by the function
;;   typically won't match the number of streams passed in. If the
;;   pattern doesn't match at all, the empty list is returned.
;;


(define *eoi-object* (read (open-input-string "")))
(define (eoi? x) (equal? x *eoi-object*))
(define (make-eoi) *eoi-object*)


;; This matches a sequence of patterns. 

(define (seq p1 p2)
  (lambda (sk fk strm)
    (p1 (lambda (strm1) (p2 sk fk strm1)) fk strm)))

;; This matches either one of two patterns. It's analogous to patterns
;; separated by the '|' in regular expressions.

(define (bar p1 p2)
  (lambda (sk fk strm)
    (p1 sk (lambda _ (p2 sk fk strm)) strm)))


;; Kleene closure. Analogous to '*' 

(define (star p)
  (lambda (sk fk strm)
        (p (lambda (strm1) 
   	    (if (eoi? (cadr strm1)) (sk strm1)
	     ((star p) sk sk strm1))) sk strm)))

;; this parser always succeeds

(define (pass sk fk s) (sk s))
  
;; Positive closure. Analogous to '+' 

(define (pos pat) (seq pat (star pat)))

;; Optional pattern. Analogous to '?' 

(define (opt pat) (bar pat pass))

;; Matches a consecutive list of patterns

(define (lst ps)
  (let ((ps (reverse ps)))
    (let recur ((ps (cdr ps)) (p1 (car ps)))
      (cond ((null? ps) p1)
	    (else (recur (cdr ps) (seq (car ps) p1)))))))
  

;; datatype used by bind and drop
(define-record-type box (make-box contents)
  box? (contents box-contents ))

(define box make-box)
(define unbox box-contents)

;; Given a list (X_1 ... X_n), returns a list ( (X_1 ... X_(n-1))  X_n )
(define-inline (split-at-last x)
  (if (null? x) (list #f (list))
      (let loop ((prev (list (car x))) (rest (cdr x)))
	(cond ((null? rest)
	       (if (null? (cdr prev))
		   (list '() (car prev))
		   (list (reverse (cdr prev)) (car prev))))
	      (else (loop (cons (car rest) prev) (cdr rest)))))))

;; helpers for bind
(define-inline (bind-apply f)
  (lambda (s)
    (cond ((pair? s)
	   (let ((eaten (car s))
		 (food  (cadr s)))
	     (let* ((ep     (split-at-last eaten))
		    (eaten1 (car ep))
		    (eaten0 (cadr ep)))
	       (assert (box? eaten0))
	       (let ((x   (and (list? eaten1) (f eaten1))))
		 (if x
                     (list (append x (unbox eaten0)) food)
		     (list (unbox eaten0) food)))
	       )))
	  (else s))))

(define-inline (box-stream s)
  (cond ((pair? s)
	 (let ((eaten (car s))
	       (food  (cadr s)))
	   (list (list (box eaten)) food)))
	(else s)))

;; Binds a procedure f to the consumed tokens returned by p
;; Calls failure on empty input
(define (bind f p)
  (let ((ba (bind-apply f)))
    (lambda (sk fk s)
      (if (eoi? (cadr s)) 
          (fk s)
	  (let ((sk1 (lambda (s1) (sk (ba s1))))
		(fk1 (lambda (s1) (fk s))))
	    (p sk1 fk1 (box-stream s)))))))

;; Same as bind, but calls success on empty input
(define (bind* f p)
  (let ((ba (bind-apply f)))
    (lambda (sk fk s)
      (if (eoi? (cadr s)) 
          (sk (ba (box-stream s)))
	  (let ((sk1 (lambda (s1) (sk (ba s1))))
		(fk1 (lambda (s1) (fk s))))
	    (p sk1 fk1 (box-stream s)))))))


(define (drop p)
  (bind (lambda x #f) p))


;; helpers for rebind
(define-inline (rebind-apply g)
  (lambda (i s) 
    (cond ((pair? s)
	   (let ((eaten (car s))
		 (food  (cdr s)))
	     (let* ((ep (split-at-last eaten))
		    (eaten1 (car ep))
		    (eaten0 (cadr ep)))
	       (assert (box? eaten0))
	       (let* ((x   (and (list? eaten1) (g i eaten1)))
		      (res (if x (cons (append x (unbox eaten0)) food) 
			       (cons (unbox eaten0) food))))
		 res))))
	  (else s))))

;; Applies a procedure f to the un-consumed tokens, then applies
;; procedure g to the result of f and the tokens returned by p
;; Calls failure on empty input
(define (rebind f g p)
  (let ((ra (rebind-apply g)))
    (lambda (sk fk s)
      (if (eoi? (cadr s)) 
          (fk s)
	  (let* ((info   ((compose f cadr) s))
		 (sk1    (lambda (s) (sk (ra info s)))))
	    (p sk1 fk (box-stream s)))))))

;; Same as rebind, but calls success on empty input
(define (rebind* f g p)
  (let ((ra (rebind-apply g)))
    (lambda (sk fk s)
      (if (eoi? (cadr s)) 
          (sk s)
	  (let* ((info   ((compose f cadr) s))
		 (sk1    (lambda (s) (sk (ra info s)))))
	    (p sk1 fk (box-stream s)))))))


;; This takes a pattern and a string, turns the string into a list of
;; streams (containing one stream), applies the pattern, and returns
;; the longest match.

(define (->char-list s)
  (if (string? s) (list (string->list s)) s))

(define (lex pat error ss)
  (let* ((stream (cond ((string? ss) `(() . ,(->char-list ss)))
		       ((pair? ss)   ss)
		       (else  (error ss)))))
    (pat (lambda (s) (list (reverse (first s)) (second s)))
	 (lambda (s) (error s)) stream)))


  
;; 'tok' builds a pattern matcher function that applies procedure p to
;; a given token and an input character. If the procedure returns a
;; true value, that value is prepended to the list of consumed
;; elements, and the input character is removed from the list of input
;; elements.

(define (tok t p)
  (lambda (sk fk strm)
    (let ((c (car strm))
          (u (cadr strm)))
      (cond ((eoi? u)    (fk strm))
            ((null? u)  (fk (list c (make-eoi))))
            ((p t (lseq-first u)) =>
             (lambda (ans) (sk (list (cons ans c) (lseq-rest u)))))
            (else  (fk strm))
            )))
  )


			       
;; Converts a binary predicate procedure to a binary procedure that
;; returns its right argument when the predicate is true, and false
;; otherwise.

(define (try p) (lambda (x y) (let ((res (p x y))) (and res y))))
  

;; Matches a single character

(define (char c) (tok c (try char=?)))
  
;; Matches any of a SRFI-14 set of characters. 

(define (set s)
    (let ((cs (if (char-set? s) s (list->char-set (if (string? s) (string->list s) s)))))
      (tok cs (try char-set-contains?))))

;; Range of characters. Analogous to character class '[]'

(define (range a b)
  (if (char<? b a) (range b a)
      (tok (ucs-range->char-set (char->integer a) (+ 1 (char->integer b))) 
           (try char-set-contains?))))

;; Matches a literal string s 

(define (lit s)
    (let ((f (lambda (t) (tok t (try char=?)))))
      (lst (map f (if (string? s) (string->list s) s)))))


)
