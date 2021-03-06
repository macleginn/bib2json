(declare (uses bibtex-parse))

(import 
	(chicken io)
	(chicken format)
	(chicken string)
	(chicken port)
	spiffy
	spiffy-request-vars
	intarweb
	uri-common
	json)

;;; process-input: Port x Integer -> String
;;; usage: (process-input port content-length) reads the input from
;;; the port and returns a list of JSON-formatted bibliographical
;;; records.
(define process-input
  (lambda (port content-length)
    (let ([input-text
	   (get-body port
		     content-length)])
      (call-with-output-string
	(lambda (port)
	  (json-write
	   (get-bibtex-records input-text)
	   port))))))

;;; get-body: Port x Integer -> String
;;; usage (get-body port content-length) reads the
;;; body of the request from the port.
(define get-body
  (lambda (port content-length)
    (let loop ([res '()]
	       [content-length content-length])
	(if (eqv? content-length 0)
	    (let ([result (reverse-list->string res)])
	       result)
	    (loop (cons (read-char port) res)
		  (sub1 content-length))))))


(define parse-input
  (lambda (continue)
    (let* ([uri (request-uri (current-request))]
	   [port (request-port (current-request))]
	   [headers (request-headers (current-request))]
	   [content-length (header-value 'content-length headers)])
      (if (equal? (uri-path uri) '(/ "parse"))
	  (with-headers
	   '()
	   (lambda ()
	     (send-response status: 'ok
			    body: (process-input port
						 content-length)
			    headers: `((access-control-allow-origin "*")
				       (content-type "application/json")))))
	  (continue)))))

(server-port 41000)
(vhost-map `(("localhost" . ,parse-input)))

(start-server)
