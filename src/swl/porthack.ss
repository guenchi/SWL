;;
;; Hack for open-input-file, to fix file access problems in Windows.
;;
;; open-input-file reads the entire input file into a string port
;; and immediately closes the file port.
;;

;; compile in subset-mode 'system and load (once)

(eval-when (compile load eval) (subset-mode 'system))

(let-syntax ((include-this-hack
              (if (memq (machine-type) '(i3nt ppcnt))
                  (syntax-rules () ((_ exp) exp))
                  (syntax-rules () ((_ exp) (void))))))

(include-this-hack
(let ((o-i-f (top-level-value 'open-input-file)))
  (define handler
    (lambda (name)
      (lambda (msg . args)
        (record-case (cons msg args)
          [block-read (p str cnt)
           (let ([b (port-input-buffer p)]
                 [i (port-input-index p)]
                 [n (port-input-size p)])
             (let ((m (fx- n i)))
               (if (fx= m 0)
                   #!eof
                   (let ((cnt (fxmin cnt m)))
                     (do ((i i (fx+ i 1)) (j 0 (fx+ j 1)))
                         ((fx= j cnt)
                          (set-port-input-index! p i)
                          cnt)
                         (string-set! str j (string-ref b i)))))))]
          [char-ready? (p) #t]
          [clear-input-port (p)
           (set-port-input-index! p (port-input-size p))]
          [close-port (p) (mark-port-closed! p)]
          [file-length (p) (string-length (port-input-buffer p))]
          [file-position (p . pos)
           (if (null? pos)
               (port-input-index p)
               (let ((pos (car pos)))
                 (if (<= 0 pos (port-input-size p))
                     (set-port-input-index! p pos)
                     (assertion-violationf 'file-position
                            "invalid position ~s for ~s"
                            pos p))))]
          [peek-char (p)
           (let ([b (port-input-buffer p)]
                 [i (port-input-index p)]
                 [n (port-input-size p)])
             (if (fx< i n)
                 (string-ref b i)
                 #!eof))]
          [port-name (p) name]
          [read-char (p)
           (let ([b (port-input-buffer p)]
                 [i (port-input-index p)]
                 [n (port-input-size p)])
             (if (fx< i n)
                 (begin
                   (set-port-input-index! p (fx+ i 1))
                   (string-ref b i))
                 #!eof))]
          [unread-char (c p)
           (let ([b (port-input-buffer p)]
                 [i (port-input-index p)])
             (if (fx> i 0)
                 (let ((i (fx- i 1)))
                   (set-port-input-index! p i)
                   (string-set! b i c))
                 (assertion-violationf 'unread-char "attempt to unread too far on ~s" p)))]
          [else (assertion-violationf #f "port operation ~s not handled by ~s"
                       (cons msg args))]))))
  (set! open-input-file
    (lambda args
      (define read-port
        (lambda (ip)
          (define bufsize 4096)
          (let f ()
            (let ([buf (make-string bufsize)])
              (let ([n (block-read ip buf bufsize)])
                (cond
                  [(eof-object? n) '()]
                  [(= n bufsize) (cons buf (f))]
                  [else (cons (substring buf 0 n) (f))]))))))
      (critical-section
        (let ((ip (apply o-i-f args)))
          (let ((s (apply string-append (read-port ip))))
            (close-input-port ip)
            (make-input-port (handler (port-name ip)) s)))))))
))

(eval-when (compile load eval) (subset-mode #f))

