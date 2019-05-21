;; Copyright (c) 1996 John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(define st:offset->index
  (lambda (text-wgt ofs)
    (send text-wgt add-offset '(0 . 0) ofs)))

(define st:index->offset
  (lambda (text-wgt idx)
    (define ballpark
      (lambda ()
	(let loop ((ofs 1))
	  (if (send text-wgt pos<=? idx (st:offset->index text-wgt ofs))
	    ofs
	    (loop (+ ofs ofs))))))
    (let* ((guess-high (ballpark))
	   (guess-low (quotient guess-high 2)))
      (define bsearch
	(lambda (low high)
	  (let* ((mid (quotient (+ low high) 2))
		 (mid-idx (st:offset->index text-wgt mid)))
	    (cond
	      [(send text-wgt pos=? idx mid-idx) mid]
	      [(send text-wgt pos<? idx mid-idx) (bsearch low (1- mid))]
	      [else (bsearch (1+ mid) high)]))))
      (bsearch guess-low guess-high))))

