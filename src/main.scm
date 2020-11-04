(declare (uses bibtex-parse))

(import srfi-13
	json)

(let ([port (current-input-port)])
	(json-write (get-records-from-port port)))