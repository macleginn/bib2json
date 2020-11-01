(import (chicken format)
	(chicken io)
	(chicken irregex)
	(chicken string)
	(chicken port))
(import utf8
	utf8-srfi-13
	utf8-srfi-14
	srfi-69)
;; (import (except scheme list->string read-char display)
;; 	(except string substring-index-ci))

(define defined-strings (make-hash-table))

;;; define-string: String -> undefined
;;; usage: (define-string str) parses a bibtex record
;;; of the form @string{ varname = "value" } and sets
;;; the value of varname in defined-strings to the value.
(define define-string
  (lambda (str)
    (if (not (string-prefix-ci "@string" str))
	(error (format "Wrong record type: ~S" str))
	(let* ([no-prefix (substring str
			       (string-length "@string{"))]
	       [no-\} (string-chomp no-prefix "}")]
	       [no-whitespace (string-trim-both no-\})]
	       [fields (map string-trim-both
			    (split-once no-whitespace "="))]
	       (if (eqv? (length fields)
			 1)
		   (error (format "No assignment found: ~S" str))
		   (hash-table-set! defined-strings
				    (string->symbol (car fields))
				    (concatenate-parts (cdr fields)))))
	  ))))


;;; split-once: String x String | Character -> Listof(String)
;;; usage (split-once str delim) splits a string in two
;;; parts: before and after the occurrence of the delimiter.
;;; If the delimiter is not found, the whole string is returned.
;; (define split-once
;;   (lambda (str delim)
;;     (cond [(char? delim)
;; 	   (let ([delim-index (string-index str delim)])
;; 	     (if (not delim-index)
;; 		 str
;; 		 (list (substring str 0 delim-index)
;; 		       (substring str (add1 delim-index)))))]
;; 	  [(string? delim)
;; 	   (let ([delim-index (substring-index delim str)]
;; 		 [delim-length (string-length delim)])
;; 	     (if (not delim-index)
;; 		 str
;; 		 (list (substring str 0 delim-index)
;; 		       (substring str (+ delim-index
;; 					 delim-length)))))]
;; 	  [else
;; 	   (error "The delimiter must be either a character or a string.")]
;; 	  )))


;;; concatenate-parts: port -> String
;;; usage: (concatenate-parts port) converts a string of the form
;;; "part1" [# " part2"]... into "part1 part2". Unlike the vanilla
;;; bibtex, it permits concatenation of tokens delimited with {}
;;; and does not need # to concatenate well-formed tokens. I.e.,
;;; sequences like `"part1" "part2" {part3}` will also be concatenated.
;;; Variables are replaced with their values from defined-strings.
;;; Undefined variables trigger an error.
;;; Should be invoked in an (call-with-input-string str concatenate-parts)
;;; context.
(define concatenate-parts
  (lambda (port)
    (let loop ([next-token (read-word port)]
	       [res '()])
      (cond [(eof-object? next-token)
	     (string-concatenate (reverse res))]
	    [(string=? next-token "#")
	     (loop ((read-word port)
		    res))]
	    [else (loop (read-word port)
			(cons next-token res))]))))

;;; read-word: Port -> String | Symbol
;;; usage: (read-word port) returns the next word contained between
;;; "" or {}, #f if the port is empty or an error if the
;;; next sequence of tokens is ill formed.
(define read-word
  (lambda (port)
    (let loop ([c (read-char port)]
	       [res '()]
	       [after-backslash #f]
	       [quotechar #f]
	       [depth 0])
      (cond [(< depth 0) (error (format "Unbalanced curly braces in the record:~%~C~A"
					quotechar
					(reverse-list->string res)))]
	    [(eof-object? c)	     
	     (cond [quotechar
		    (error (format "An unfinished token:~%~C~A"
				   quotechar
				   (reverse-list->string res)))]
		   [else
		    (if (null? res)
			c
			(let ([token (reverse-list->string res)])
			  (if quotechar
			      token
			      (string->symbol token))))])]
	    [after-backslash
	     (loop (read-char port)
		   (cons c res)
		   #f
		   quotechar
		   depth)]
	    [(not quotechar)		; Outside-token mode
	     (cond [(eqv? c #\space)	
		    (if (null? res)	
			(loop (read-char port) ; Ignore spaces outside tokens
			  res
			  after-backslash
			  quotechar
			  depth)
			(string->symbol (reverse-list->string res)))] ; Return a variable name
		   [(or (eqv? c #\") (eqv? c #\{)) ; Start collecting a token
		    (let ([start-depth (if (eqv? c #\") 0 1)])
		      (if (null? res)
			  (loop (read-char port)
				res
				after-backslash
				c
				start-depth)
			  (error (format "A malformed string literal: ~A"
					 (reverse-list->string (cons c res))))))]
		   [(eqv? c #\#)	; Return token separator
		    (if (null? res)
			"#"
			(error (format "An illegal variable name: ~A"
				       (reverse-list->string (cons c res)))))]
		   [(or (char-set-contains? char-set:letter c)
			(char-set-contains? char-set:digit c))
		    (loop (read-char port) ; Start collecting a variable name or add to it
			  (cons c res)
			  after-backslash
			  quotechar
			  depth)]
		   [else
		    (error (format "An illegal character outside a token: ~C"
				c))])]
	    [else			; Inside-token mode
	     (cond [(eqv? c #\{)	; Cases where the quotechar choice is irrelevant
			(loop (read-char port)
			      (cons c res)
			      after-backslash
			      quotechar
			      (add1 depth))]
		   [(eqv? c #\\)
			(loop (read-char port)
			      (cons c res)
			      #t
			      quotechar
			      depth)]
		   [else
		    (if (eqv? quotechar #\")	; Quote-mark-delimited token
			(cond [(eqv? c #\})
			       (loop (read-char port)
				     (cons c res)
				     after-backslash
				     quotechar
				     (sub1 depth))]
			      [(eqv? c #\")
			       (if (> depth 0)
				   (error (format "Unbalanced curly braces in the record:~%~A"
						  (reverse-list->string res)))
				   (reverse-list->string res))]		     
			      [else (loop (read-char port)
					  (cons c res)
					  after-backslash
					  quotechar
					  depth)])
			(cond [(eqv? c #\})	; Curly-brace delimited token
			       (if (eqv? depth 1)
				   (reverse-list->string res)
				   (loop (read-char port)
					 (cons c res)
					 after-backslash
					 quotechar
					 (sub1 depth)))]		       
			      [else (loop (read-char port)
					  (cons c res)
					  after-backslash
					  quotechar
					  depth)]))])]))))



;;; parse-bibtex-record: String -> HashTable(String, SchemeValue)
;;; usage: (parse-bibtex-record) parses an individual record into the following
;;; representation (in JSON notation):
;;; { "type": "book"|"article"|"incollection", &c
;;;     "description": {
;;;         "key": ..., "author": ... } }

;;; get-bibtex-entry: Port -> String | #f
;;; usage: (get-bibtex-entry port) returns the next bibtex
;;; entry from the input port or #f if there are no more records
;;; and raises an error if there is an unfinished record such as
;;; @book{xxx,
;;; @preamble, @string, and @<record-type> entries
;;; are returned. Comments between entries are ignored.
;;; The internal structure of the entries is disregarded and
;;; may contain mistakes. Any sequence @\w+{...} with balanced curly
;; braces inside the outer ones will be happily returned.
(define get-bibtex-entry
  (lambda (port)
    (let loop ([c (read-char port)])
      (cond [(eof-object? c)
	     #f] ; We went throught the file without seeing any records
	    [(eqv? c #\@)
	     (get-entry-type '(#\@) port)]	; Start accumulating the record
	    [else
	     (loop (read-char port))]))))

;;; get-entry-type: ListOf(Character) x Port -> String
;;; usage: This procedure is used internally to append an entry
;;; type to the bibtex record being collected. It will throw
;;; an error if it encounters a non-letter symbol before the
;;; record begins or if there is no entry type at all.
(define get-entry-type
  (lambda (res port)
    (let loop ([c (read-char port)]
	       [res res])
      (cond [(eof-object? c)
	     (error "An incomplete record at the end of input.")]
	    [(and (or (eqv? c #\() (eqv? c #\{))
		  (eqv? (length res) 1))
	     (error (string-concatenate '("An entry without a type was encountered."
					  " Check for '@{' or '@(' in the input.")) )]
	    [(or (eqv? c #\()
		 (eqv? c #\{))
	     (get-entry-body (cons c res)
			     c  ; The outer brace type
			     1  ; Embedding level
			     #f ; Not inside quotes
			     #f ; Not after a backslash
			     port
			     )]
	    [(eqv? c #\space)		; Skip whitespace
	     (loop (read-char port) res)]
	    [(char-set-contains? char-set:letter c)
	     (loop (read-char port) (cons c res))]
	    [else error (format "A non-letter character in the entry type: ~S." c)])
      )))

;;; get-entry-body: LisOf(Character) x Bool x Bool x Port -> String
;;; usage: This procedure is used internally to collect the body of
;;; a bibtex entry.
(define get-entry-body
  (lambda (res
	   outer-delimiter		; This never changes so we don't track it in the loop
	   embedding-level
	   inside-quotes
	   after-backslash
	   port)
    (let loop ([c (read-char port)]
	       [res res]
	       [embedding-level embedding-level]
	       [inside-quotes inside-quotes]
	       [after-backslash after-backslash])
      (cond [(eof-object? c)
	     (error "An incomplete record at the end of input.")]
	    [after-backslash		; Just add the character
	     (loop (read-char port)
		   (cons c res)
		   embedding-level
		   inside-quotes
		   #f)]
	    [(and (eqv? c #\@)		; This symbol cannot appear inside a record
		                        ; but not inside a field.
		  (eqv? embedding-level 1))
	     (error (format "An erroneus @ inside a record or an incomplete record:~%~S"
			    (reverse-list->string (cons c res))))]
	    ;; Unconditionally add the next character after a backslash
	    ;; if the embedding level is right or we're between
	    ;; quote marks. Signal an error otherwise.
	    [(eqv? c #\\)
	     (if (or inside-quotes
		     (> embedding-level 1))
		 (loop (read-char port)
		       (cons c res)
		       embedding-level
		       inside-quotes
		       #t)
		 (error (format "Backslash outside a field:~%~S"
				(reverse-list->string (cons c res)))))]
	    [(and (or (eqv? c #\}) (eqv? c #\))) ; We're done if the parens/braces match
		  (eqv? embedding-level 1))
	     (if (or (and (eqv? c #\} )
			  (eqv? outer-delimiter #\{ ))
		     (and (eqv? c #\) )
			  (eqv? outer-delimiter #\( )))
		 (reverse-list->string (cons c res))
		 (error (format "Unbalanced braces/parens in the record:~%~S"
				(reverse-list->string (cons c res)))))]
	    [(eqv? c #\})	        ; Decrease the embedding level
	     (loop (read-char port)
		   (cons c res)
		   (sub1 embedding-level)
		   inside-quotes
		   after-backslash)]
	    [(eqv? c #\{)	        ; Increase the embedding level
	     (loop (read-char port)
		   (cons c res)
		   (add1 embedding-level)
		   inside-quotes
		   after-backslash)]
	    [(eqv? c #\")		; Toggle the inside-quotes mode
	     (loop (read-char port)
		   (cons c res)
		   embedding-level
		   (not inside-quotes)
		   after-backslash)]
	    [(and (eqv? c #\space)
		  (eqv? (car res) #\space)) ; Skip redundant whitespace
	     (loop (read-char port)
		   res
		   embedding-level
		   inside-quotes
		   after-backslash)]
	    [else (loop (read-char port)
			(cons c res)
			embedding-level
			inside-quotes
			after-backslash)]))))

(let loop ()
  (let ([record (get-bibtex-entry (current-input-port))])
    (if record (begin (display record)
		      (display "\n\n")
		      (loop)))))
