(declare (unit bibtex-parse))

(import (chicken format)
	(chicken io)
	(chicken irregex)
	(chicken string)
	(chicken port)
	utf8
	utf8-srfi-13			; string routines
	utf8-srfi-14			; charsets
	srfi-12				; exception handling
	srfi-69				; hash tables
	json)



;;; parse-bibtex-record: String x HashTable(Symbolx,String) -> HashTable(String, SchemeValue)
;;; usage: (parse-bibtex-record) parses an individual record into the
;;; following representation (in JSON notation):
;;; {
;;;   "cite-key": "stevens1989quantal",
;;;   "entry-type": "article",
;;;   "description": {
;;;     "author": "Stevens, Kenneth N.",
;;;     "publisher": "Elsevier",
;;;     "journal": "Journal of Phonetics",
;;;     "volume": "17",
;;;     "year": "1989",
;;;     "pages": "3--45",
;;;     "title": "On the quantal nature of speech",
;;;     "number": "1-2"
;;;   }
;;; }
;;; In the current package, the records to parse will be extracted
;;; using get-bibtex-entry and are well-formed as regards
;;; the outer syntax, i.e. they are of the form @TYPE{} or @TYPE()
;;; and have balanced curly braces inside them. The procedure therefore 
;;; assumes that the prefix is well-formed. It also strips the final } or )
(define parse-bibtex-record
  (lambda (str defined-strings)
    (let ([no-whitespace (string-trim-both str)])
      (call-with-input-string
	(substring no-whitespace
		   0
		   (sub1 (string-length no-whitespace)))
	(lambda (port)
	  (get-record-type port
			   (make-hash-table)
			   no-whitespace
			   defined-strings))))))


;;; get-record-type: Port x
;;;                  HashMap(Symbol, SchemeValue) x
;;;                  String x
;;;                  HashMap(Symbol, String)
;;;                  -> HashMap(Symbol, SchemeValue)
;;; usage: (get-record-type port result buffer c stage) adds the type of
;;; bibtex record to the hash table and calls the procedure that extracts
;;; the fields.
(define get-record-type
  (lambda (port result original-string defined-strings)
    (let loop ([c (read-char port)]
	       [buffer '()])
      (cond [(or (eqv? c #\{)
		 (eqv? c #\())
	     ;; Add the type to the hash table.
	     ;; Proceed to the next stage.
	     (begin (hash-table-set! result
				     'entry-type
				     (reverse-list->string buffer))
		    (get-record-fields port
				       result
				       original-string
				       defined-strings))]
	    [(eqv? c #\@)
	     (loop (read-char port)
		   buffer)]
	    [else
	     (loop (read-char port)
		   (cons c buffer))]))))


;;; get-record-fields: Port x
;;;                    HashMap(String, SchemeValue) x
;;;                    -> HashMap(String, SchemeValue)
;;; usage: (get-record-fields port result buffer) fills the
;;; description of the bibtex entry.
(define get-record-fields
  (lambda (port result original-string defined-strings)
    ;; We begin by extracting the cite key, which
    ;; is a string of characters before the comma.
    ;; This will break if there is an escaped comma
    ;; inside the cite-key.
    (let ([cite-key (read-token
		     (lambda (x) (not (eqv? x #\,)))
		     port)])
      (hash-table-set! result
		       'cite-key
		       (string-trim-both cite-key)))
    ;; Now we extract the fields.
    ;; Each field begins with a field name followed
    ;; by the = sign. We extract the field name,
    ;; extract tokens, and concatenate them
    ;; if needed. When we encounter a comma, we proceed
    ;; to the next field.
    (let ([description (make-hash-table)])
      (let outer-loop ([next-token (string-trim-both
				    (read-token
				     (lambda (x) (not (eqv? x #\=)))
				     port))])
	; No good reason not to allow trailing commas or empty records.
	(if (string-null? next-token) 
	    (begin (hash-table-set! result
				  'description
				  description)
		   result)
	    (let ([field-name next-token])
	      ;; Extract the value.
	      (let inner-loop ([next-token (read-word port original-string)]
			       [buffer '()])
		(cond [(symbol? next-token) ; Try looking up in the hash table.
		       (inner-loop (read-word port original-string)
				   (cons (hash-table-ref defined-strings next-token
							 (lambda ()
							   (symbol->string next-token)))
					 buffer))]
		      [(eof-object? next-token) ; End of fields.
		       (if (null? buffer)
			   (abort `(parse-error
				    ,(format "~S~%An empty value in the field ~A"
					     original-string
					     field-name)))
			   (begin (hash-table-set! description
						   (string->symbol field-name)
						   (string-concatenate (reverse buffer)))
				  (hash-table-set! result
						   'description
						   description)
				  result))]
		      [(string=? next-token ",") ; Move on to the next field.
		       (if (null? buffer)
			   (abort `(parse-error
				    ,(format "~S~%An empty value in the field ~A"
					     original-string
					     field-name)))
			   (begin (hash-table-set! description
						   (string->symbol field-name)
						   (string-concatenate (reverse buffer)))
				  (outer-loop (string-trim-both
					       (read-token
						(lambda (x) (not (eqv? x #\=)))
						port)))))]
		      
		      [(string=? "#" next-token)
		       (inner-loop (read-word port original-string)
				   buffer)]
		      [else (inner-loop (read-word port original-string)
					(cons next-token buffer))]))))))))


;;; define-string: String x HashTable(Symbol,String) -> undefined
;;; usage: (define-string str defined-strings) parses a bibtex record
;;; of the form @string[({] varname = "value" [})] and sets
;;; the value of 'varname in defined-strings to the value.
(define define-string
  (lambda (str defined-strings)
    (if (not (string-prefix-ci? "@string" str))
	(abort `(definition-error ,(format "Wrong record type:~%~A" str)))
	(let* ([no-prefix (substring str
				     (string-length "@string "))]
	       [no-quotechar (substring no-prefix
					0
					(sub1 (string-length no-prefix)))]
	       [no-whitespace (string-trim-both no-quotechar)]
	       [fields (map string-trim-both
			    (split-once no-whitespace "="))])
	       (if (eqv? (length fields)
			 1)
		   (abort `(definition-error ,(format "No assignment found:~%~A" str)))
		   (hash-table-set! defined-strings
				    (string->symbol (car fields))
				    (call-with-input-string
					(cadr fields)
				      (lambda (port) (concatenate-parts
						      port
						      str
						      defined-strings)))))))))


;;; split-once: String x String | Character -> Listof(String)
;;; usage (split-once str delim) splits a string in two
;;; parts: before and after the occurrence of the delimiter.
;;; If the delimiter is not found, the whole string is returned.
(define split-once
  (lambda (str delim)
    (cond [(char? delim)
	   (let ([delim-index (string-index str delim)])
	     (if (not delim-index)
		 str
		 (list (substring str 0 delim-index)
		       (substring str (add1 delim-index)))))]
	  [(string? delim)
	   (let ([delim-index (substring-index delim str)]
		 [delim-length (string-length delim)])
	     (if (not delim-index)
		 str
		 (list (substring str 0 delim-index)
		       (substring str (+ delim-index
					 delim-length)))))]
	  [else
	   (error "The delimiter must be either a character or a string.")])))


;;; concatenate-parts: Port x HashTable(String,String) -> String
;;; usage: (concatenate-parts port) converts a string of the form
;;; "part1" [# " part2"]... to "part1 part2". Unlike the vanilla
;;; bibtex, it permits concatenation of tokens delimited with {}
;;; and does not need # to concatenate well-formed tokens. I.e.,
;;; sequences like `"part1" "part2" {part3}` will also be concatenated.
;;; Variables are replaced with their values from the defined-strings map.
;;; Undefined variables trigger an error.
;;; Should be invoked in an (call-with-input-string str concatenate-parts)
;;; context.
(define concatenate-parts
  (lambda (port original-string defined-strings)
    (let loop ([next-token (read-word port
				      original-string)]
	       [res '()])
      (cond [(eof-object? next-token)
	     (string-concatenate (reverse res))]
	    [(symbol? next-token)
	     (loop (read-word port
			      original-string)
		   (cons (hash-table-ref defined-strings next-token)
			 res))]
	    [(string=? next-token "#")
	     (loop (read-word port
			      original-string)
		   res)]
	    [(string=? next-token ",")
	     (abort `(parse-error "A comma in the string variable."))]
	    [else (loop (read-word port
				   original-string)
			(cons next-token res))]))))


;;; read-word: Port x String -> String | Symbol
;;; usage: (read-word port original-string)
;;; returns the next word contained between
;;; "" or {} or #f if the port is empty and
;;; raises an exception if the next sequence of tokens
;; is ill formed.
(define read-word
  (lambda (port original-string)
    (let loop ([c (read-char port)]
	       [res '()]
	       [after-backslash #f]
	       [quotechar #f]
	       [depth 0])
      ;; Check if the next char is a comma and we are not inside a string.
      (let ([comma-test (peek-char port)])
	(if (and (not quotechar)
		 (eqv? comma-test #\,))
	    (if (and (null? res) (char-set-contains? char-set:whitespace c))
		(begin (read-char)	; Consume and return the comma
		       ",")
		; Return the variable name without consuming the comma
		(string->symbol (reverse-list->string (cons c res))))
	    ; The basic mode
	    (cond [(< depth 0)
		   (abort
		    `(parse-error
		      ,(format "~S~%Unbalanced curly braces in the record:~%~C~A"
			       original-string
			       quotechar
			       (reverse-list->string res))))]
		  [(eof-object? c)	     
		   (cond [quotechar
			  (abort `(parse-error
				   ,(format "~S~%An unfinished token:~%~C~A"
					    original-string
					    quotechar
					    (reverse-list->string res))))]
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
		   (cond [(char-set-contains? char-set:whitespace c)	
			  (if (null? res)	
			      (loop (read-char port) ; Ignore spaces outside tokens
				    res
				    after-backslash
				    quotechar
				    depth)
			      ; Return a variable name
			      (string->symbol (reverse-list->string res)))] 
			 [(or (eqv? c #\") (eqv? c #\{)) ; Start collecting a token
			  (let ([start-depth (if (eqv? c #\") 0 1)])
			    (if (null? res)
				(loop (read-char port)
				      res
				      after-backslash
				      c
				      start-depth)
				(abort `(parse-error
					 ,(format "~S~%A malformed string literal: ~A"
						  original-string
						  (reverse-list->string (cons c res)))))))]
			 [(eqv? c #\#)	; Return token separator
			  (if (null? res)
			      "#"
			      (abort `(parse-error
				       ,(format "~S~%An illegal variable name: ~A"
						original-string
						(reverse-list->string (cons c res))))))]
			 [(eqv? c #\,)
			  ","]
			 [(or (char-set-contains? char-set:letter c)
			      (char-set-contains? char-set:digit c))
			  (loop (read-char port) ; Start collecting a variable name or add to it
				(cons c res)
				after-backslash
				quotechar
				depth)]
			 [else
			  (abort `(parse-error
				   ,(format "~S~%An illegal character outside a token: ~C"
					    original-string
					    c)))])]
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
					 (abort
					  `(parse-error
					    ,(format
					      "~S~%Unbalanced curly braces in the record:~%~A"
					      original-string
					      (reverse-list->string res))))		 					 (reverse-list->string res))]		     
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
						depth)]))])]))))))


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
	     (abort
	      `(parse-error
		,(format "An incomplete record at the end of input:~%~A"
			 (reverse-list->string res))))]
	    [(and (or (eqv? c #\() (eqv? c #\{))
		  (eqv? (length res) 1))
	     (abort `(parse-error
		      ,(string-concatenate '("An entry without a type was encountered."
					  " Check for '@{' or '@(' in the input."))))]
	    [(eqv? c #\{)
	     (get-entry-body (cons c res)
			     c  ; The outer brace type
			     1  ; Embedding level
			     #f ; Not inside quotes
			     #f ; Not after a backslash
			     port
			     )]
	    [(eqv? c #\()
	     (get-entry-body (cons c res)
			     c  ; 
			     0  ; Parens do not increase embedding
			     #f ; 
			     #f ; 
			     port
			     )]
	    [(char-set-contains? char-set:whitespace c)	; Skip whitespace
	     (loop (read-char port) res)]
	    [(char-set-contains? char-set:letter c)
	     (loop (read-char port) (cons c res))]
	    [else
	     (abort `(parse-error
		      ,(format "A non-letter character in the entry type: ~S." c)))]))))


;;; get-entry-body: ListOf(Character) x Bool x Bool x Port -> String
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
	     (abort `(parse-error
		      ,(format "An incomplete record at the end of input:~%~S"
			       (reverse-list->string res))))]
	    [(< embedding-level 0)
	     (abort `(parse-error
		      ,(format "Unbalanced braces in the record:~%~S"
			       (reverse-list->string res))))]
	    [after-backslash		; Just add the character
	     (loop (read-char port)
		   (cons c res)
		   embedding-level
		   inside-quotes
		   #f)]
	    [(and (eqv? c #\@)		; This symbol cannot appear inside a record
		                        ; without being inside a string.
		  (not inside-quotes)
		  (not (eqv? outer-delimiter #\())
		  (eqv? embedding-level 1))
	     (abort `(parse-error
		      ,(format "An erroneus @ inside a record or an incomplete record:~%~S"
			    (reverse-list->string (cons c res)))))]
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
		 (abort `(parse-error
			  ,(format "Backslash outside a field:~%~S"
				(reverse-list->string (cons c res))))))]
	    [(or			; Are we done?
	      ;; Outer paren case
	      (and (eqv? outer-delimiter #\( )
		   (eqv? c #\))
		   (eqv? embedding-level 0))
	      ;; Outer brace case
	      (and (eqv? outer-delimiter #\{ )
		   (eqv? c #\})
		   (eqv? embedding-level 1)))
	     (reverse-list->string (cons c res))]
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


;;; json->string: SchemeValue -> String
;;; usage (json->string val) returns the JSON
;;; representation of an object.
;;; It can handle whatever write-json can handle.
(define json->string
  (lambda (val)
    (call-with-output-string
      (lambda (port)
	(json-write val port)))))


;;; get-bibtex-records: String -> Listof(HashTable(Symbol,SchemeValue))
;;; usage: (get-bibtex-records input) parses the input string into
;;; a list of hash tables with bilbiographical descriptions.
(define get-bibtex-records
  (lambda (input)
    (call-with-input-string
	input
	get-records-from-port)))


;;; TODO: docstring
(define (get-records-from-port port)
  (let ([defined-strings (make-hash-table)])
    (let loop ([done '()]
	       [failed '()])
      (handle-exceptions exn
	  (cond [(eqv? (car exn) 'parse-error)
		 (loop done
		       (cons (cadr exn) failed))]
		[(eqv? (car exn) 'definition-error)
		 (loop done
		       (cons (cadr exn) failed))]
		[else (abort exn)])
	(let ([record (get-bibtex-entry port)])
	  (cond [(not record)
		 (list (reverse done) (reverse failed))]		  
		[(string-prefix-ci? "@preamble" record)
		 (loop done failed)]
		[(string-prefix-ci? "@string" record)
		 (begin (define-string record defined-strings)
			(loop done failed))]
		[else
		 (loop (cons (parse-bibtex-record record
						  defined-strings)
			     done)
		       failed)]))))))
