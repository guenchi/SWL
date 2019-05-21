(define design:lookahead-tokens '())

(define design:peek-token
  (lambda (ofs p)
    (cond
      [(assq p design:lookahead-tokens) =>
       (lambda (t)
	 (apply (lambda (type token start end)
		  (values type token (+ ofs start) (+ ofs end)))
	   (cdr t)))]
      [else 
	(mvlet ((type token start end) (read-token p))
	  (set! design:lookahead-tokens
	    (cons (weak-cons p (list type token start end))
		  design:lookahead-tokens))
	  (values type token (+ ofs start) (+ ofs end)))])))

(define design:read-token
  (lambda (ofs p)
    (let ((t (assq p design:lookahead-tokens)))
      (if t
	(begin
	  (set! design:lookahead-tokens (remq t design:lookahead-tokens))
	  (apply (lambda (type token start end)
		   (values type token (+ ofs start) (+ ofs end)))
	     (cdr t)))
	(mvlet ((type token start end) (read-token p))
	  (values type token (+ ofs start) (+ ofs end)))))))
