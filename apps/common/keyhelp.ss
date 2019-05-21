;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

(define keyhelp
  (lambda ()
    (define-swl-class (<keyhelper> parent) (<label> parent)
      (ivars)
      (inherited)
      (inheritable)
      (private)
      (protected)
      (public
        [key-press (k mods)
         (if (char? k)
             (printf "key = ~s~n" k)
             (let ([x (keycode->keys k)])
               (if (char? x)
                   (printf "That key matches the character ~s~n" x)
                   (printf "That key matches the symbol(s) ~s~n" x)))) ]))
    (let* ((top (create <toplevel> with (title: "Key Help")))
           (help (create <keyhelper> top with (title: "Press a key"))))
      (show help)
      (set-focus help))))
(keyhelp)
