(import (chicken format)
	(chicken io)
	(chicken irregex))
(import utf8
	utf8-srfi-14)
(import (except scheme list->string read-char display))

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
	    [(and (eqv? c #\{)
		  (eqv? (length res) 1))
	     (error "An entry without a type was encountered. Check for '@{' in the input.")]
	    [(eqv? c #\{)
	     (get-entry-body (cons #\{ res)
			     1  ; Curly-brace-embedding level
			     #f ; Not inside quotes
			     #f ; Not after a backslash
			     port
			     )]
	    [(eqv? c #\space)		; Skip whitespace
	     (loop (read-char port) res)]
	    [(char-set-contains? char-set:letter c)
	     (loop (read-char port) (cons c res))]
	    [else error (format "A non-letter character in the entry type: ~S." c)]))))

;;; get-entry-body: LisOf(Character) x Bool x Bool x Port -> String
;;; usage: This procedure is used internally to collect the body of
;;; a bibtex entry.
(define get-entry-body
  (lambda (res
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
	     (error "An erroneus @ inside a record or an incomplete record!")]
	    ;; Unconditionally add the next character
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
		 (error "Backslash outside a field."))]
	    [(and (eqv? c #\})		; We're done
		  (eqv? embedding-level 1))
	     (list->string (reverse (cons c res)))]
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
