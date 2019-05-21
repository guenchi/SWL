;; Copyright (c) 1996 John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(require "../common/scrollframe.ss")
;(require "../common/arrows.ss")
;(require "../common/auxio.ss")
;(require "../common/warning-dialog.ss")
;(require "../common/app.ss")
;(require "../repl/repl-text.ss")

#; ; was cute but no point now
(define-swl-class (<traffic-light>) (<command-menu-item>)
  (ivars
    (color 'green)
    (red-title #f)
    (green-title #f)
    (show-light #t))
  (inherited)
  (inheritable)
  (private
    [get-image (name)
     (guard (c [#t #f])
       (create <photo> with
         (filename: (format "~a/~a" (getenv "SWL_ROOT") name))))]
    [update ()
     (when show-light
       (set-title! self
         (case color
           [(red) red-title]
           [(green) green-title]
           [else "bug in repl.ss"]))
       (swl:sync-display))])
  (protected)
  (public
    [init ()
     (send-base self init)
     (let ((broken? (memq (machine-type) '(i3nt ppcnt))))
       (if broken?
           (begin
             (set! red-title "Busy")
             (set! green-title "Ready"))
           (begin
             (set! red-title (or (get-image "redlight.gif") "Busy"))
             (set! green-title (or (get-image "grnlight.gif") "Ready")))))]
    [set-color! (clr)
     (set! color clr)
     (update)]
    [toggle ()
     (set! show-light (not show-light))
     (update)
     (unless show-light (set-title! self ""))]
    [get-color () color]))
  
(define-swl-class (<repl-toplevel> start-k end-k) (<app-toplevel>)
  (ivars [repl #f])
  (inherited mini-buffer mini-search)
  (inheritable mini-buffer mini-search)
  (private)
  (protected)
  (public
   [insert-expression (str) (send repl insert-expression str)]
   [init (start-k end-k)
    (send-base self init)
    (send self load-prefs 'repl)
    (let () ;;; needed for defns
     (define-syntax simple-menu-item
       (syntax-rules ()
         ((_ str1 str2 act (argname argval) ...)
          (create <command-menu-item> with
                  (title: (cons str1 str2))
                  (action: act)
                  (argname argval) ...
                  ))))
     (transcript-output-port #f)
     ;;
     ;; widget bindings
     ;; 
     (let* ((scrolled-frame
             (create <scrollframe> self
                     with (default-vscroll: #t) (sticky-hscroll: #t)))
            (history-counter
             (create <command-menu-item>)))

       (set! repl
         (create <repl-text> scrolled-frame with
           (background-color: 'white)
           (foreground-color: 'black)))

       (send repl set-font!
         (send self get-pref 'base-font (create <font> 'courier 12 '())))

       (send self notify-text repl)

       ;;
       ;; action and menu-item bindings
       ;;
       (letrec
           ([transcript-menu-item
             (create <command-menu-item> with (title: "") ; set by <repl-text>
               (action:
                 (lambda (item)
                   (send repl toggle-transcript self))))]
            [apphis-menu-action
              (lambda (item)
                (thread-fork
                 (lambda ()
                   (let ((name (swl:file-dialog "Read history from file" 'open
                                 (parent: self)
                                 (default-dir: (current-directory)))))
                     (if name
                         (let ((ip (open-input-file name)))
                           (let loop ((hi (send repl history-get)))
                             (let loop2 ((item ""))
                               (let ((str (swl:read-line ip)))
                                 (if str
                                     (if (string=? "" str)
                                         (loop (cons item hi))
                                         (if (string=? "" item)
                                             (loop2 str)
                                             (loop2 (string-append
                                                     item
                                                     "
"
                                                     str))))
                                     (send repl history-set!
                                           (if (string=? item "")
                                               hi
                                               (cons item hi)))
                                     ))))
                           (close-port ip)
                           #t)
                         #f)
                     ))))]
            [savhis-menu-action
              (lambda (item)
                (let ((hi (reverse (send repl history-get))))
                  (thread-fork
                   (lambda ()
                     (let ((name (swl:file-dialog "Save history as" 'save
                                   (parent: self)
                                   (default-dir: (current-directory)))))
                       (if name
                           (let ((op (open-output-file name 'truncate)))
                             (for-each
                              (lambda (str)
                                (display str op)
                                (display #\newline op)
                                (display #\newline op)
                                )
                              hi)
                             (close-port op)
                             #t)
                           #f)
                       )))))]
            [quit-menu-item
             (simple-menu-item
              "_Close" "Alt+q"
              (lambda (item) (send self destroy)))]
            [new-menu-item
             (simple-menu-item "_New file" "Alt+n" (lambda (item) (new-edit)))]
            [open-menu-item
             (simple-menu-item "_Open file..." "Alt+o"
               (lambda (item)
                 (let ([filename
                        (swl:file-dialog "Select a file to edit" 'open
                          (file-types: '(("Scheme source" ("*.ss"))
                                         ("All files" ("*"))))
                          (parent: self))])
                   (when filename (new-edit filename)))))]
            [load-menu-item
             (simple-menu-item "_Load..." ""
               (lambda (item)
                 (let ([filename
                        (swl:file-dialog "Select a file to load" 'open
                          (file-types: '(("Scheme source" ("*.ss"))
                                         ("All files" ("*"))))
                          (parent: self))])
                   (when filename
                     (send repl insert-expression
                       (format "(load ~s)~%" filename))))))]
            [cd-menu-item
             (simple-menu-item "Change Directory..." "Alt+d"
               (lambda (item)
                 (let ([dirname (swl:tcl-eval '|tk_chooseDirectory|
                                  '-parent self
                                  '-mustexist #t)])
                   (unless (equal? dirname "")
                     (send repl insert-expression
                       (format "(cd ~s)\n" dirname))))))]
            [undo-menu-item
             (simple-menu-item "_Undo" "Ctrl+z"
               (lambda (item) (send repl undo)))]
            [redo-menu-item
             (simple-menu-item "_Redo" "Alt+z"
               (lambda (item) (send repl redo)))]
            [copy-menu-item
             (simple-menu-item
              "_Copy"  "Alt+c"
              (lambda (item) (send repl action-copy)))]
            [cut-menu-item
             (simple-menu-item
               "Cu_t"
               "Alt+x"
               (lambda (item) (send repl action-cut)))]
            [paste-menu-item
             (simple-menu-item
               "_Paste"
               "Alt+v"
               (lambda (item) (send repl action-paste)))]
            [search-forward-menu-item
             (simple-menu-item
               "Search _Forward"
               "Ctrl+s"
               (lambda (item) (send repl search-forward)))]
            [search-backward-menu-item
             (simple-menu-item
               "Search _Backward"
               "Ctrl+r"
               (lambda (item) (send repl search-backward)))]
            ) ;;; action and menu-item bindings

         ;;
         ;; menu bindings
         ;;
         (let*
             ((menu-file
               (create <cascade-menu-item> with
                 (title: "_File")
                 (menu:
                  (create <menu>
                   (list (simple-menu-item "New _repl" ""
                           (lambda (item) (new-repl)))
                         new-menu-item
                         open-menu-item
                         load-menu-item
                         transcript-menu-item
                         (create <cascade-menu-item> with
                           (title: "_History")
                           (menu:
                             (make-menu
                               ("_Read from file" apphis-menu-action)
                               ("_Save to file" savhis-menu-action)
                               ("_Clear History"
                                 (lambda (item)
                                   (send repl history-clear))))))
                         cd-menu-item
                         quit-menu-item
                         (simple-menu-item "E_xit SWL" ""
                           (lambda (item)
                             (swl:end-application 'exit-all))))
                   ))))
              (menu-edit
               (create <cascade-menu-item> with
                 (title: "_Edit")
                 (menu:
                  (create <menu>
                   (list undo-menu-item
                         redo-menu-item
                         copy-menu-item
                         cut-menu-item
                         paste-menu-item
                         (simple-menu-item "_Go to line..." "Ctrl+g"
                           (lambda (item)
                             (send repl ask-goto-line)))
                         (simple-menu-item "Forward S-expression" "Ctrl+0"
                           (lambda (item)
                             (send repl move-to-match 'forward)))
                         (simple-menu-item "Backward S-expression" "Ctrl+9"
                           (lambda (item)
                             (send repl move-to-match 'backward)))
                         (simple-menu-item "Select S-expression Forward" "Ctrl+)"
                           (lambda (item)
                             (send repl select-to-match 'forward)))
                         (simple-menu-item "Select S-expression Backward" "Ctrl+("
                           (lambda (item)
                             (send repl select-to-match 'backward)))
                         search-forward-menu-item
                         search-backward-menu-item
                         )))))
              (menu-control 
               (create <cascade-menu-item> with
                 (title: "Contro_l")
                 (menu:
                  (create <menu>
                   (list
                    (simple-menu-item
                     "_Interrupt"    "Ctrl+c"
                     (lambda (item)
                       (send repl interrupt)))
                    (simple-menu-item
                     "_Clean environment..."    ""
                     (lambda (item)
                       (let ([ok '|Clean|]
                             [no-warn '|Clean, don't warn again|]
                             [cancel '|Cancel|]
                             [als (swl:load-preferences 'warnings)])
                         (let ([ans
                                (if (cond
                                      [(assq 'clean-env als) => cdr]
                                      [else #t])
                                    (warning-dialog self
                                      "Cleaning the environment will discard any global definitions that have not been saved in a file.\n\nTo save all the expressions entered in this REPL to a file, use File-->History-->Save to file.  Normally you would create a file containing your programs using the Editor, and use the REPL for testing.\n\nNote that the expressions you have entered can be recalled and re-evaluated via the REPL's history mechanism (see Help) even if you clean the environment."
                                        `(,ok ,no-warn ,cancel))
                                    ok)])
                           (when (eq? ans no-warn)
                             (critical-section
                               (let ([als (swl:load-preferences 'warnings)])
                                 (swl:save-preferences! 'warnings
                                   (let ([a (assq 'clean-env als)])
                                     (if a
                                         (begin (set-cdr! a #f) als)
                                         (cons (cons 'clean-env #f) als)))))))
                           (unless (eq? ans cancel)
                             (send repl insert-expression
                               "(clean-environment)\n")))))))))))
              (menu-preferences 
               (create <cascade-menu-item> with
                 (title: "_Preferences")
                 (menu:
                   (make-menu
                     ("_Font..."
                       (lambda (item)
                         (swl:font-dialog self
                           "Select a font for REPL text"
                           (swl:font-families 'fixed)
                           '(6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 22 24 26 32)
                           '(bold normal)
                           (lambda () (send repl get-font))
                           (lambda (fnt)
                             (when fnt
                               (send repl set-font! fnt)
                               (send self set-pref! 'base-font fnt))))))
                     ("_Toggles"
                       (create <menu>
                         (list (create <check-menu-item> with
                                 (title: "The _Box")
                                 (enabled: #f) ; existing box implement breaks UNDO/REDO
                                 (prefs-key: 'use-the-box)
                                 (action:
                                   (lambda (item)
                                     (send repl show-the-box (send item get-selected)))))
                               (create <check-menu-item> with
                                 (title: "_Wrap Text")
                                 (prefs-key: 'wrap-text)
                                 (action:
                                   (lambda (item)
                                     (send repl set-wrap!
                                       (if (send item get-selected) 'word 'none)))))
                               (create <check-menu-item> with
                                 (title: "_Auto-indent")
                                 (selected: (send repl get-auto-indent))
                                 (prefs-key: 'auto-indent)
                                 (action:
                                   (lambda (item)
                                     (send repl set-auto-indent!
                                       (send item get-selected))))))))
                     ("_Save preferences"
                       (lambda (ignore)
                         (send self save-prefs! 'repl)))))))
              ) ;;; menu bindings

           (send repl set-menu-items!
                 transcript-menu-item
                 undo-menu-item
                 redo-menu-item
                 copy-menu-item
                 cut-menu-item
                 paste-menu-item
                 new-menu-item
                 open-menu-item
                 cd-menu-item
                 (list
                   load-menu-item
                   cd-menu-item
                   transcript-menu-item)
                 )
             
           (pack scrolled-frame (expand: #t) (fill: 'both))
           (pack repl (expand: #t) (fill: 'both))
           (set-menu! self
             (create <menu>
               (list menu-file
                     menu-edit
                     menu-control
                     menu-preferences
                     history-counter
                     (swl:help-menu))))
           (set-border-width! (get-menu self) 0)
           (set-relief! (get-menu self) 'flat)
#;         (send traffic-light toggle)
#;         (send repl set-traffic-light! traffic-light)
           (thread-fork
            (lambda ()
              (thread-name "Repl")
              ;;; (set! myrepl repl) ;;; for debugging
              (thread-become-server!) ;;; cafe will set the reset handler
              (send repl repl-new-cafe history-counter)
              (send self destroy)
              (end-k)))
           (start-k repl)))))
     (void)]
  )
)

(define new-repl
  (case-lambda
    [() (new-repl (gensym) (lambda args (void)) void)]
    [(key) (new-repl key (lambda args (void)) void)]
    [(start-k end-k) (new-repl (gensym) start-k end-k)]
    [(key start-k end-k)
     ; we set swl:repl-key in two places:
     ;  1) a newly introduced thread group, to ensure that fallback actions
     ;     use the correct repl key (e.g., editors opened via menus)
     ;  2) the application thread itself, so that invoking new-edit in the
     ;     repl thread itself uses the proper repl key
     (thread-fork-group
       (lambda ()
         (swl:repl-key key)
         (swl:begin-application
           (lambda (token)
             (swl:repl-key key)
             (let ([top (create <repl-toplevel> start-k end-k with
                          (title: "repl")
                          (destroy-request-handler:
                            (lambda (self)
                              (swl:remove-repl key)
                              (swl:end-application token))))])
               (swl:insert-repl key top)
               (lambda () (send top destroy)))))))
     (void)]))

;(swl:register-application "New Repl" new-repl)

