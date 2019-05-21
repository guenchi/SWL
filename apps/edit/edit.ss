;;
;; Copyright (c) 1996-1998 John Zuckerman
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
(require "../common/app.ss")
(require "../common/scrollframe.ss")
(require "../common/auxio.ss")
(require "edit-text.ss")

; new-repl should probably change editor's value of the repl key.  But to
; do that correctly we need to wrap the begin-application in a
; thread-fork-group.  But to do that, we need do change to logic for
; returning (via continuation) to the caller of new-repl.

; when/if we get modules set up for this file, abstract the port-opening code
; and make backup filename a preference

(define-swl-class (<edit-toplevel> iop filename notify-q app-token) (<app-toplevel>)
  (ivars
    (current-filename filename)
    (file-port iop)
    (backup? (and filename #t))
   )
  (inherited mini-buffer mini-search)
  (inheritable mini-buffer mini-search)
  (private
    [set-current-filename! (name)
     (set! current-filename name)
     (set-title! self (if name (format "Editing ~s" name) "Editor"))]
    [delegate (who handler)
     (let ([txt (send self get-text-widget)])
       (if txt (handler txt) (assertion-violationf who "no text widget installed")))])
  (protected)
  (public
   [get-filename () current-filename]
   [register-request (request)
    (delegate 'register-request
      (lambda (x) (send x register-request request)))]
   [unregister-request (request)
    (delegate 'unregister-request
      (lambda (x) (send x unregister-request request)))]
   [set-locked! (locked?)
    (delegate 'set-locked!
      (lambda (x) (send x set-enabled! (not locked?))))]
   [get-buffer-content ()
    (delegate 'get-buffer-content
      (lambda (x) (send x get-string '(0 . 0) 'end)))]
   [show-sexpression-at (pos bfp efp)
    (delegate 'show-sexpression-at
      (lambda (x) (send x show-sexpression-at pos bfp efp)))]
   [show-explanation (str)
    (delegate 'show-explanation
      (lambda (x) (send x display-mini str)))]
   [init (iop filename notify-q app-token)
    (send-base self init)
    (send self load-prefs 'edit)
    (set-current-filename! filename)
    (let () ;;; needed for defns
      (define-syntax simple-menu-item
        (syntax-rules ()
          ((_ (str1 str2) act (argname argval) ...)
           (create <command-menu-item> with
                   (title: (cons str1 str2))
                   (action: act)
                   (argname argval) ...
                   ))))

      (define block-copy
        (lambda (ip consumer)
          (define bufsize 8192)
          (let ([buf (make-string bufsize)])
            (let loop ()
              (let ([n (block-read ip buf bufsize)])
                (unless (eof-object? n)
                  (consumer (if (< n bufsize) (substring buf 0 n) buf))
                  (loop)))))))

      ;;
      ;; widget bindings
      ;;
      (let* ((scrolled-frame
              (create <scrollframe> self with
                      (default-vscroll: #t)
                      (sticky-hscroll: #t)
                      ))
             (edit-wgt
              (create <edit-text> scrolled-frame with
                      (background-color: 'white)
                      (foreground-color: 'black)
                      (wrap: 'none)
                      ))
             ) ;;; widget bindings

        (define open-new-file
          (lambda (filename)
            (on-error (with-message msg
                        (warning-dialog self
                          (format "Unable to open ~s.\n\n~a" filename msg)))
              (let ([iop (on-error
                           (let ([p (open-input-file filename)])
                             (warning-dialog self
                               (format "Unable to open ~a for writing.\n\nYou can edit this file but must save it under a different filename." filename))
                             p)
                           (open-input-output-file filename))])
                (set! file-port iop)
                (set-current-filename! filename)
                (load-the-file)))))

        (define load-the-file
          (lambda ()
            (when (input-port? file-port)
              (let ([end (send edit-wgt get-end-mk)])
                (block-copy file-port
                  (lambda (string)
                    (send edit-wgt raw-insert-at end string))))
              (send edit-wgt set-cursor-pos! '(0 . 0)))))

        (define save-to-file
          (lambda (filename iop backup?)
           ; get buffer content before trashing any files in case of error
            (let ([buf (send edit-wgt get-string '(0 . 0) 'end)])
             ; write backup file
             ; might want an override dialog to allow save to continue
             ;   even if backup fails
              (when (and backup? (> (file-length iop) 0))
                (file-position iop 0)
                (let ([backup-filename (string-append filename ".backup")])
                  (let ([backup-port
                         (on-error
                           (assertion-violationf #f "cannot open backup file ~a for writing" backup-filename)
                           (open-output-file backup-filename 'truncate))])
                    (block-copy iop
                      (lambda (s)
                        (block-write backup-port s (string-length s))))
                    (close-output-port backup-port))))
             ; write buffer contents, buf contains 1 extra char at end
              (truncate-file iop)
              (block-write iop buf (- (string-length buf) 1))
              (flush-output-port iop)
             ; clear buffer modified flag
              (send edit-wgt set-buffer-to-safe!))))

        (define save-the-file
          (lambda ()
            (on-error (with-message msg (warning-dialog self (format "Unable to save the file.\n\n~a" msg)))
              (save-to-file current-filename file-port backup?)
             ; at most one backup per file
              (set! backup? #f))))

        (define save-as-file
          (lambda ()
            (let ([filename (swl:file-dialog "Save file as" 'save
                              (file-types: '(("Scheme source" ("*.ss"))
                                             ("All files" ("*"))))
                              (parent: self)
                              (default-dir: (current-directory)))])
              (when filename
                (on-error (with-message msg
                            (warning-dialog self
                              (format "Unable to save as ~s.\n\n~a" filename msg)))
                  (let ([iop (open-input-output-file filename)])
                   ; wait to install until we're sure there were no errors saving
                    (on-error (with-message msg (warning-dialog self (format "Unable to save to ~a.\n\n~a" filename msg)))
                      (save-to-file filename iop #t)
                     ; at most one backup per file
                      (set! backup? #f)
                      (when (port? file-port) (close-port file-port))
                      (set! file-port iop)
                      (set-current-filename! filename))))))))

        (send edit-wgt set-font!
          (send self get-pref 'base-font (create <font> 'courier 12 '())))

        (send self notify-text edit-wgt)

        (send self set-destroy-request-handler!
         (let ([posted? #f])
          (lambda (self)
            (and (not posted?)
                 (or (not (send edit-wgt buffer-modified?))
                     (fluid-let ([posted? #t])
                       ; make sure the <toplevel> window is visible
                       ; and on top so we know which editor we're
                       ; about to blow away.
                       (send self show)
                       (send self raise)
                       (swl:sync-display)
                       (let ([ans
                              (warning-dialog self
                                (format
                                  "\n     The ~a is not saved to disk.\n     Do you really want to discard your changes?\n"
                                  (if current-filename
                                      (format "file ~s" current-filename)
                                      "edit buffer"))
                                '(|discard changes| |keep editing|))])
                         (eq? ans '|discard changes|))))
                 (not (send edit-wgt reserved? '|close anyway|))
                 (begin
                   (when file-port (close-port file-port))
                   (swl:remove-editor self)
                   (swl:end-application app-token)
                   #t)))))

        ;;
        ;; action & menu-item bindings
        ;;
        (letrec
            ((buffer-modified-menu-item (create <command-menu-item>))
             
             (new-action (lambda (item) (new-edit)))

             (open-action
               (lambda (item)
                 (let ([filename
                        (swl:file-dialog "Select a file to edit" 'open
                          (file-types: '(("Scheme source" ("*.ss"))
                                         ("All files" ("*"))))
                          (parent: self))])
                   (when filename
                    ; if already editing a file, open in a new window
                     (if (or current-filename (send edit-wgt buffer-modified?))
                         (new-edit filename)
                         (open-new-file filename))))))

             (insert-file-action
               (lambda (item)
                 (let ([filename
                        (swl:file-dialog "Insert file" 'open
                          (file-types: '(("Scheme source" ("*.ss"))
                                         ("All files" ("*"))))
                          (parent: self))])
                   (when filename
                     (on-error
                       (with-message msg
                         (warning-dialog self
                           (format "Unable to insert ~a.\n\n~a" filename msg)))
                       (let ([ip (open-input-file filename)]
                             [pos (send edit-wgt get-cursor-pos)]
                             [op (open-output-string)])
                         (block-copy ip
                           (lambda (s)
                             (block-write op s (string-length s))))
                         ; use plain insert-at to record in undo history
                         (send edit-wgt insert-at pos (get-output-string op))
                         (send edit-wgt set-cursor-pos! pos)
                         (send edit-wgt set-buffer-to-modified!)))))))

             (directory-action
               (lambda (item)
                 (let ([dirname (swl:tcl-eval '|tk_chooseDirectory|
                                  '-parent self
                                  '-mustexist #t)])
                   (unless (equal? dirname "")
                     (let ([key (swl:repl-key)])
                       (let ([repl (swl:lookup-repl key)])
                         (if repl
                             (send repl insert-expression
                               (format "(cd ~s)\n" dirname))
                             (on-error (with-message msg (warning-dialog self (format "Unable to change directory to ~a.\n\n~a" dirname msg)))
                               (cd dirname)))))))))
  
             (save-action
              (lambda (item)
                (if (and current-filename (output-port? file-port))
                    (save-the-file)
                    (save-as-file))))

             (save-as-action
              (lambda (item)
                (save-as-file)))

             (new-menu-item
              (simple-menu-item ("_New file" "Alt+n")
                                (lambda (item) (new-action item))))

             (open-menu-item
              (simple-menu-item ("_Open file..." "Alt+o")
                                (lambda (item) (open-action item))))

             (insert-file-menu-item
              (simple-menu-item ("_Insert file..." "Alt+i")
                                (lambda (item) (insert-file-action item))))

             (directory-menu-item
              (simple-menu-item ("Change _Directory..." "Alt+d")
                                (lambda (item) (directory-action item))))

             (save-menu-item
              (simple-menu-item ("_Save" "Alt+s")
                                (lambda (item) (save-action item))))

             (save-as-menu-item
              (simple-menu-item ("Save _as..." "Alt+a")
                                (lambda (item) (save-as-action item))))

             (quit-menu-item
              (simple-menu-item ("_Close" "Alt+q")
                                (lambda (item) (send self destroy))))

             (execute-menu-item
              (simple-menu-item ("(Save and) _Load" "Alt+g")
                (let ([doit
                       (lambda (repl)
                         (send repl insert-expression
                           (format "(load ~s)\n" current-filename)))])
                  (lambda (item)
                    (critical-section
                      (if current-filename
                          (when (send edit-wgt buffer-modified?)
                            (if (output-port? file-port)
                                (save-the-file)
                                (save-as-file)))
                          (save-as-file))
                     ; proceed only if save was successful
                      (when (and current-filename (not (send edit-wgt buffer-modified?)))
                        (let ([key (swl:repl-key)])
                          (let ([repl (swl:lookup-repl key)])
                            (if repl
                                (doit repl)
                                (new-repl key doit void))))))))))

             (undo-menu-item
               (simple-menu-item ("_Undo" "Ctrl+z")
                 (lambda (item) (send edit-wgt undo))))

             (redo-menu-item
               (simple-menu-item ("_Redo" "Alt+z")
                 (lambda (item) (send edit-wgt redo))))

             (copy-menu-item
               (simple-menu-item ("_Copy" "Alt+c")
                 (lambda (item) (send edit-wgt action-copy))))

             (cut-menu-item
               (simple-menu-item ("Cu_t" "Alt+x")
                 (lambda (item) (send edit-wgt action-cut))))

             (paste-menu-item
               (simple-menu-item ("_Paste" "Alt+v")
                 (lambda (item) (send edit-wgt action-paste))))

             (search-forward-menu-item
               (simple-menu-item ("Search _Forward" "Ctrl+s")
                 (lambda (item) (send edit-wgt search-forward))))

             (search-backward-menu-item
               (simple-menu-item ("Search _Backward" "Ctrl+r")
                 (lambda (item) (send edit-wgt search-backward))))
             ) ;;; action & menu-item bindings
          ;;
          ;; menu bindings
          ;;
          (let*
              ((menu-file
                 (create <cascade-menu-item> with
                   (title: "_File")
                   (menu:
                     (create <menu>
                       (list (simple-menu-item ("New _repl" "")
                               (lambda (item) (new-repl)))
                             new-menu-item
                             open-menu-item
                             insert-file-menu-item
                             execute-menu-item
                             save-menu-item
                             save-as-menu-item
                             directory-menu-item
                             quit-menu-item
                             (simple-menu-item ("E_xit SWL" "")
                               (lambda (item)
                                 (swl:end-application 'exit-all))))))))
               (menu-edit
                 (create <cascade-menu-item> with
                   (title: "_Edit")
                   (menu:
                     (create <menu>
                       (list 
                             undo-menu-item
                             redo-menu-item
                             copy-menu-item
                             cut-menu-item
                             paste-menu-item
                             (simple-menu-item ("_Go to line..." "Ctrl+g")
                               (lambda (item)
                                 (send edit-wgt ask-goto-line)))
                             (simple-menu-item ("Forward S-expression" "Ctrl+0")
                               (lambda (item)
                                 (send edit-wgt move-to-match 'forward)))
                             (simple-menu-item ("Backward S-expression" "Ctrl+9")
                               (lambda (item)
                                 (send edit-wgt move-to-match 'backward)))
                             (simple-menu-item ("Select S-expression Forward" "Ctrl+)")
                               (lambda (item)
                                 (send edit-wgt select-to-match 'forward)))
                             (simple-menu-item ("Select S-expression Backward" "Ctrl+(")
                               (lambda (item)
                                 (send edit-wgt select-to-match 'backward)))
                             (simple-menu-item ("Select All" "")
                               (lambda (item)
                                 (send edit-wgt select-range '(0 . 0) 'end)))
                             search-forward-menu-item
                             search-backward-menu-item)))))
               (menu-preferences 
                 (create <cascade-menu-item> with
                   (title: "_Preferences")
                   (menu:
                     (make-menu
                       ("_Font..."
                         (lambda (item)
                           (swl:font-dialog self "Select a font for editor text"
                             (swl:font-families 'fixed)
#;
                             '(-8 -10 -12 -14 -16 -18 -20 -22 -24
                                8 10 12 14 16 18 20 22 24)
'(6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 22 24 26 32)
                             '(bold normal)
                             (lambda () (send edit-wgt get-font))
                             (lambda (fnt)
                               (when fnt
                                 (send edit-wgt set-font! fnt)
                                 (send self set-pref! 'base-font fnt))))))
                       ("_Toggles"
                         (create <menu>
                           (list (create <check-menu-item> with
                                   (title: "The _Box")
                                   (enabled: #f) ; existing box implement breaks UNDO/REDO
                                   (prefs-key: 'use-the-box)
                                   (action:
                                     (lambda (item)
                                       (send edit-wgt show-the-box (send item get-selected)))))
                                 (create <check-menu-item> with
                                   (title: "_Auto-indent")
                                   (selected: (send edit-wgt get-auto-indent))
                                   (prefs-key: 'auto-indent)
                                   (action:
                                     (lambda (item)
                                       (send edit-wgt set-auto-indent!
                                         (send item get-selected)))))
                                 )))
                       ("_Save preferences"
                         (lambda (ignore)
                           (send self save-prefs! 'edit)))))))
; Due to bug in Tk 8.0.4 on Unix, command-menu-items are never invoked
; if drawn in a menubar (ie. as menu: option of a <toplevel>).
; I have a patch to tk8.0.4/library/menu.tcl that fixes this, but instead
; plan to make Help a cascade.
;                 (create <command-menu-item> with
;                   (title: "Help")
;                   (action:
;                     (lambda (item)
;                       (let ([root (getenv "SWL_ROOT")])
;                         (if (not root)
;                             (warning-dialog self "Can't locate documentation.  (SWL_ROOT environment variable not set)" 'oops)
;                             (swl:open-url (string-append "file:" root "/edit.html")))))))
               ) ;;; menu bindings
              
            (send edit-wgt set-menu-items!
                  buffer-modified-menu-item
                  new-menu-item open-menu-item insert-file-menu-item
                  execute-menu-item
                  save-menu-item save-as-menu-item
                  directory-menu-item
                  quit-menu-item
                  undo-menu-item
                  redo-menu-item
                  copy-menu-item
                  cut-menu-item
                  paste-menu-item
                  #f
                  )

            (send self set-menu!
              (create <menu>
                (list menu-file
                      menu-edit
                      menu-preferences
                      buffer-modified-menu-item
                      (swl:help-menu))))
            ; hack (until we fix menu.ss, menu configuration has to be done
            ;       after the menu is installed via set-menu!)
            (set-relief! (send self get-menu) 'flat)
            (set-border-width! (send self get-menu) 0)
            (pack scrolled-frame (expand: #t) (fill: 'both))
            (thread-fork-group
              (lambda ()
                (load-the-file)
               ; Code that calls new-edit shouldn't get the editor instance
               ; until the specified file, if any, has been loaded.  If we
               ; don't guarantee this, then methods like show-sexpression-at
               ; may show bogus source or no source at all.
               ; Once we've loaded the file, if any, send this instance back
               ; to the continuation waiting to receive it from the message
               ; queue in new-edit, below.
                (thread-send-msg notify-q self)))
            ;; This makes the edit-wgt responsive to key bindings on startup
            ;; we should fix SWL so this isn't needed.
            (send edit-wgt set-focus)
            )))
      (swl:add-editor self)
      (void))]
   )
  )

(define new-edit
  (rec new-edit
    (case-lambda
      [() (new-edit #f)]
      [(current-filename) (new-edit current-filename (lambda (ed) (void)))]
      [(current-filename k)
      ; Using #%$fixed-path? here isn't going to work because it considers
      ; relative paths fixed.  The best plan is to open an input-output port
      ; right here and use truncate-file when it's time to rewrite it.
      ;
      ; We open the ports here in case we're being called via command-line
      ; arguments passed to a server that is running in a different current
      ; directory from the directory of the invoking client.  The server
      ; parameterizes current directory to point to the directory of the
      ; client while we process the command-line arguments, so we open the
      ; ports while we have access to the files, lest someone change the
      ; current directory before we think to create our backup file, zB.
      ;
      ; Opening the backup file with 'append so that we don't modify the file
      ; in case they don't save the current file.
       (define colon-path?
         (lambda (filename i)
           (and (memq (machine-type) '(i3nt ppcnt))
                (fx= i 1)
                (char=? (string-ref filename 1) #\:)
                (char-alphabetic? (string-ref filename 0)))))
       (define path-sep?
         (lambda (filename i)
           (let ([c (string-ref filename i)])
             (or (char=? c #\/) (colon-path? filename i)))))
       (define split-path
         (lambda (filename)
           (let ([len (string-length filename)])
             (let loop ([i (fx- len 1)])
               (if (fx< i 0)
                   (values "" filename)
                   (if (path-sep? filename i)
                       (values
                         (substring filename 0 (fx+ i 1))
                         (substring filename (fx+ i 1) len))
                       (loop (fx- i 1))))))))
       (define absolute?
         (lambda (path)
           (let ([len (string-length path)])
             (case (machine-type)
               [(i3nt ppcnt)
                (and (> len 2)
                     (let ([prefix (substring path 0 2)])
                      ; not treating \foo or /foo as absolute since those paths
                      ; are relative to the currently selected drive letter
                       (or (string=? prefix "//")  ; //cfs.indiana.edu
                           (and (> len 3)
                                (colon-path? path 1)
                                (char=? (string-ref path 2) #\/)))))]
               [else (> len 1) (char=? (string-ref path 0) #\/)]))))
       (define join-paths
         (lambda (p1 p2)
           (if (and (>= (string-length p2) 2) (colon-path? p2 1))
               p2
               (let ([len (string-length p1)])
                 (if (and (> len 0) (path-sep? p1 (- len 1)))
                     (string-append p1 p2)
                     (string-append p1 "/" p2))))))
       (define sanitize
         (lambda (s)
           (import scheme)
           (case (machine-type)
             [(i3nt ppcnt)
              (list->string (subst #\/ #\\ (string->list s)))]
             [else s])))
      ; Thanks to Windows for a few dozen special cases.
      ; We convert \ to / for political and religious reasons.
      ; Known "bug":  on Windows, (new-edit "/foo") is relative to current
      ; directory rather than being root of current drive.  Yawn.
       (define absolute-path
        ; assume leading whitespace is intended part of filename
         (lambda (filename bail-out)
           (let ([filename (sanitize filename)])
             (if (absolute? filename)
                 filename
                 (let-values ([(path file) (split-path filename)])
                   (on-error (begin
                               (warning-dialog #f
                                 (format "Unable to determine absolute path for file ~a"
                                   filename))
                               (or (swl:file-dialog "Select a file to edit" 'open
                                     (file-types:
                                       '(("Scheme source" ("*.ss"))
                                         ("All files" ("*"))))
                                     (default-file: file))
                                   (bail-out)))
                     (join-paths
                      ; let the system normalize the path for us
                       (parameterize ([current-directory
                                       (join-paths (current-directory) path)])
                         (sanitize (current-directory)))
                       file)))))))
       (call/cc
         (lambda (bail-out)
          ; swl:file-dialog gives us absolute paths, so we just have to
          ; ensure that we get an absolute path when started up via new-repl
           (let ([current-filename
                  (and current-filename
                       (absolute-path current-filename bail-out))])
             (let ([iop (and current-filename
                          (on-error
                            (let ([p (open-input-file current-filename)])
                              (warning-dialog 'noblock
                                (format "Unable to open ~a for writing.\n\nYou can edit this file but must save it under a different filename." current-filename))
                              p)
                            (open-input-output-file current-filename)))])
               (let ([q (thread-make-msg-queue "new-edit")])
                 (swl:begin-application
                   (lambda (token)
                     (let ([editor (create <edit-toplevel> iop current-filename q token)])
                       (lambda () (send editor destroy)))))
                ; wait for the editor to initialize itself before returning
                ; maybe this would be better handled via lock method
                 (k (thread-receive-msg q)))))))])))
 
; (swl:register-application "New Editor" new-edit)
