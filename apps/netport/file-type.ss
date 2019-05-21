;; Copyright (c) 1996 Carl Bruggeman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Heuristic file-type checker


(define file-type
  (lambda (filename)
    (let ([s (make-string 512)]
          [ip (open-input-file filename)])
      (let ([n (block-read ip s 512)])
        (close-input-port ip)
        (cond
          [(eof-object? n) 'EMPTY]
          [(and (>= n 6) (string-ci=? (substring s 0 3) "GIF"))
           (read (open-input-string (substring s 0 5)))]
          [(and (>= n 10) (string-ci=? (substring s 6 10) "JFIF"))
           'JPEG]
          [(and (>= n 8) (string-ci=? (substring s 0 8) "#DEFINE "))
           'BITMAP]
          [(let loop ([i 0])
             (if (fx= i n)
                 #t
                 (and (< 0 (char->integer (string-ref s i)) 128)
                      (loop (fx+ i 1)))))
           'ASCII]
          [else 'DATA])))))

 
;(pretty-print (file-type "ex/larrow.bmp"))
;(pretty-print (file-type "ex/LH.gif"))
;(pretty-print (file-type "ex/bob.jpg"))
;(pretty-print (file-type "ex/proj4.html"))
