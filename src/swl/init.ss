;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;; call before doing any widget stuff

; rkd csv7.0 hack to get things working again with new import semantics.
; rkd swl1.0a obviated with with-implicit in define-swl-class (syntax.ss)
#|
(import swl:module-setup)
(import swl:oop)
(import swl:macros)
(import swl:option)
(import swl:generics)
(import swl:threads)
|#

(define swl:with-library-file
 ; no idea why Tk insists on such ugly dialogs...  suppose I could cons up my own
 ; should provide graceful exodus, of course, but I have no time
  (lambda (name k)
    (define warn
      (lambda (title msg)
        (swl:tcl-eval 'tk_dialog '.swlerror title msg 'warning 0 "Drat")
        (format "library file ~s not found" name)))
    (let ([lib (getenv "SWL_ROOT")])
      (if (not lib)
          (warn "Configuration error"
            "Environment variable SWL_ROOT is not set.  Cannot locate library files.")
          (let ([path (string-append lib "/" name)])
            (if (file-exists? path)
                (k path)
                (warn "Installation error"
                  (format "Cannot locate SWL library file ~s in ~s" name lib))))))))

(define swl:error-log-port)
(define swl:show-balloon-help)
(define swl:hide-balloon-help)

(define swl:open-url
 ; use native web browsers
  (lambda (url)
    (case (machine-type)
      [(i3nt ti3nt ppcnt)
       (when (<= (swl:open-dde-file url "" "") 32)
         (assertion-violationf #f "unable to start web browser"))]
      [(ppcosx tppcosx i3osx ti3osx) (system (format "/usr/bin/open ~a" url))]
      [else
       (let ([als (swl:load-preferences 'external-applications)])
         (define with-preference
           (lambda (key ok? k)
             (let ([a (assq key als)])
               (and a
                    (or (and (list? a) (= (length a) 2) (ok? (cadr a)))
                        (begin
                          (warningf #f
                            "Ignoring malformed ~s entry in external-applications section of .swlprefs file" a)
                          #f))
                    (on-error (with-message msg (begin (warningf #f "~a" msg) #f))
                      (k (cadr a)))))))
          (define start-browser
            (lambda (browser)
              (define first
               ; if browser is a string like "kfmclient openURL" we don't want
               ; /usr/bin/which to look for both kfmclient and openURL.
                (lambda (x)
                  (if (not (string? x))
                      x
                      (list->string
                        (let upto ([ls (string->list x)])
                          (if (or (null? ls) (char-whitespace? (car ls)))
                              '()
                              (cons (car ls) (upto (cdr ls)))))))))
              (or (case browser
                    [(firefox netscape mozilla)
                     (zero? (system (format "exec ~a -remote \"openurl(~a,new-window)\" >/dev/null 2>/dev/null" browser url)))]
                    [else #f])
                  (zero? (system (format "which ~a >/dev/null 2>/dev/null && (~a ~a&)" (first browser) browser url))))))
         (or (with-preference 'open-url-command string?
               (lambda (cmd-str)
                 (zero? (system (format "~a &" (format cmd-str url))))))
             (with-preference 'web-browser (lambda (x) (or (symbol? x) (string? x)))
               start-browser)
             (ormap start-browser '(firefox mozilla netscape))
             (let ([q (thread-make-msg-queue "get-browser")])
               (let* ([top (create <toplevel> with
                             (title: "Select default web browser")
                             (destroy-request-handler:
                               (lambda (t)
                                 (thread-send-msg q #f)
                                 #t)))]
                      [labl (create <label> top with (background-color: 'white))]
                      [save-default #f]
                      [ent (create <entry> top)]
                      [chk (create <checkbutton> top with (title: "Save as default")
                             (action: (lambda (b) (set! save-default (not save-default)))))]
                      [f1 (create <frame> top)]
                      [ok (create <button> f1 with
                            (title: "Okay")
                            (action: (lambda (b) ((get-action ent) ent))))]
                      [cancel (create <button> f1 with
                                (title: "Cancel")
                                (action: (lambda (b) (destroy top) (thread-send-msg q #f))))])
                 (define update-label!
                   (lambda (choice)
                     (set-title! labl
                       (format "\nUnable to start ~a.\n\nPlease enter the path to your\npreferred browser in the box below."
                         choice))))
                 (define loop
                   (lambda (choice)
                     (if (start-browser choice)
                         (begin
                           (thread-send-msg q #t)
                           (when save-default
                             (swl:save-preferences! 'external-applications
                               (let ([a (assq 'web-browser als)])
                                 (if (and a (list? a) (= (length a) 2))
                                     (begin
                                       (set-car! (cdr a) choice)
                                       als)
                                     (cons (list 'web-browser choice) als)))))
                           (destroy top))
                         (update-label! choice))))
                 (pack labl (side: 'top) (expand: #t) (fill: 'both))
                 (pack ent (side: 'top) (expand: #t) (fill: 'x))
                 (if save-default (send chk select) (send chk deselect))
                 (pack chk (side: 'top) (expand: #t) (fill: 'x))
                 (pack f1 (side: 'top) (expand: #t) (fill: 'x))
                 (pack ok (side: 'left) (expand: #t) (fill: 'x))
                 (pack cancel (side: 'left) (expand: #t) (fill: 'x))
                 (update-label! "firefox, mozilla, or netscape")
                 (set-action! ent
                   (lambda (e)
                     (let ([s (get-string e)])
                       (if (andmap char-whitespace? (string->list s))
                           (delete-all self)
                           (loop s))))))
               (thread-receive-msg q))
             (warning-dialog 'noblock (format "Unable to start web browser to display requested URL ~a." url))))])))

(module ()

(import swl:module-setup)

(define new-viewer
  (lambda (filename)
    (swl:open-url (format "file://~a" filename))))

(define initialize-graphics-lib
  (let ()
     (define swl:make-error-log-port
       (lambda ()
         (define q (thread-make-msg-queue 'show-error))
         (let ([top #f] [txt #f])
           (define make-new-top
             (lambda (x)
               (define dismissed? #f)
               (swl:begin-application
                 (lambda (token)
                   (set! top
                     (create <toplevel> with
                       (title: "SWL Notification")
                       (min-size: 100 100)
                       (destroy-request-handler:
                         (lambda (t)
                           (and dismissed?
                             (let ([destroy? (thread-make-msg-queue 'destroy?)])
                               (thread-send-msg q destroy?)
                               (thread-receive-msg destroy?)
                               (swl:end-application token)
                               #t))))))
                   (let ([sf
                          (create <scrollframe> top with
                            (default-vscroll: #t)
                            (sticky-hscroll: #t))]
                         [button
                          (create <button> top with
                            (title: "Dismiss")
                            (action:
                              (lambda (b)
                                (set! dismissed? #t)
                                (send top destroy))))])
                     (pack sf (side: 'top) (fill: 'both) (expand: #t))
                     (pack button (side: 'top))
                     (set! txt
                       (create <text> sf with
                         (height/char: 5)
                         (width/char: 70)
                         (wrap: 'none)
                         (background-color: 'white)))
                     (send txt insert x)
                     (send txt set-enabled! #f)
                     (send txt show)
                     (send top show)
                     (send button set-focus))
                   (lambda () (swl:end-application token))))))
           (thread-fork
             (rec loop
               (lambda ()
                 (let ([x (thread-receive-msg q)])
                  ; x is either a message or a queue waiting for permission to
                  ; destroy the widget
                   (if (string? x)
                       (if (not top)
                           (make-new-top x)
                           (begin
                             (send txt set-enabled! #t)
                             (send txt insert x)
                             (send txt set-enabled! #f)
                             (show top)
                             (send top raise)))
                       (begin
                         (set! top #f)
                         (set! txt #f)
                         (thread-send-msg x #t))))
                 (loop)))))
         (let ([op (open-output-string)])
           (define handler
             (lambda (xmsg . xargs)
               (record-case (cons xmsg xargs)
                 [flush-output-port (p)
                  (thread-send-msg q (get-output-string op))]
                 [write-char (c p) (write-char c op)]
                 [port-name (p) "swl:error-log-port"]
                 [else (let ((args (cdr xargs)))
                        (apply (port-handler op) xmsg op args))])))
           (make-output-port handler ""))))
    (lambda ()
      (swl:tcl-init 1 1)
      (swl:tcl-eval 'wm 'withdraw #\.)
     ; get tcl not to change focus on tab
      (swl:tcl-eval 'bind 'all '<|Key-Tab|> '())
      (swl:tcl-eval 'bind 'all '<|Shift-Key-Tab|> '())
      (when (let ([v (string->number (swl:tcl-eval 'set 'tcl_version))])
              (and v (< v 8.4)))
        (swl:with-library-file "menu.tcl" (lambda (patch) (swl:tcl-eval 'source patch))))

      ;; hopefully this protects us somewhat from Tk's "send" security hole
      (swl:raw-tcl-eval "tk appname \"\"")
; doesn't exist under Windows NT (in Tk4.2)
;     (swl:raw-tcl-eval "rename send \"\"")

      ;; If we restore the binding for <Destroy>, modify the callback-proc
      ;; for destroyer (above) so that it returns swl:system-queue and a widget
      ;; handle to us.  Basically that means changing #f above to
      ;; "swl:system-queue %W".
      ;;;     (swl:tcl-eval 'bind '|SWL| '|<Destroy>| destroyer)

      ;; small hack here.
      ;; s_eval will return to us as a fallback whatever args we've indicated
      ;; here.  The first is the index of the destroy callback in the callback
      ;; table.  The second is a token used by swl:event-dispatch to dispatch
      ;; this callback to the appropriate queue. In this case, swl:system-queue
      ;; is a token that points us to a special queue where each fallback is
      ;; evaluated in a separate thread (so none can clog the system queue).
      ;; Finally, s_eval returns to us the handle of the window we need to
      ;; send the destroy message.
      (set! swl:make-destroy-notify
        (let ([destroyer
               (send (make <callback-proc>
                           (lambda (widget)
                             (let ([widget (swl:lookup-widget widget)])
                               (when widget (send widget destroy))))
                           #f #t)
                     init)])
          (lambda (handle)
            (format
              "s_eval ~a swl:system-queue ~a"
              (send destroyer index-of)
              handle))))

      (let ((q (thread-make-msg-queue 'idle)))
        (thread-fork-group
         (lambda ()
           (thread-name "System Fallback")
           (call/cc thread-become-server!)
           (let loop ()
             (let ([fb (thread-receive-msg q)])
              (thread-fork
                (lambda () (swl:apply-callback-proc (car fb) (cdr fb))))
             (loop))))
         (thread-default-quantum)
         -1 ;;; run with high priority
         )
         ; (maybe insert-widget isn't the best name in the world)
         (swl:insert-widget 'swl:system-queue #f (make-swl:fbq q 'swl:system-queue)))

      (let ([balloon #f] [thread #f])
         (define make-balloon
           (lambda ()
'            (let ([b (create <balloon>)])
               (send b hide)
               (swl:sync-display)
               (set! balloon b))))
        (set! swl:balloon-help-delay
          (make-parameter
            1000
            (lambda (x)
              (unless (and (fixnum? x) (not (fxnegative? x)))
                (assertion-violationf 'swl:balloon-help-delay "invalid delay ~s" x))
              x)))
        (set! swl:show-balloon-help
          (lambda (content x y width height)
(printf "balloon-help disabled for now~n")
'           (thread-critical-section
              (unless balloon (make-balloon))
              (when thread (thread-kill thread))
              (set! thread
                (thread-fork-group
                  (lambda ()
                    (thread-sleep (swl:balloon-help-delay))
                    (show balloon content x y width height)))))))
        (set! swl:hide-balloon-help
          (lambda ()
'           (thread-critical-section
              (when thread (thread-kill thread))
              (when balloon (send balloon hide))
              (set! thread #f)))))

      (swl:tcl-eval 'bind '|SWL| '|<Configure>| configure-cbproc)
      (swl:tcl-eval 'bind '|SWL| '|<Enter>| enter-cbproc)
      (swl:tcl-eval 'bind '|SWL| '|<Leave>| leave-cbproc)
      (swl:tcl-eval 'bind '|SWL| '|<KeyPress>| keypress-cbproc)
      (swl:tcl-eval 'bind '|SWL| '|<KeyRelease>| keyrelease-cbproc)
      (swl:tcl-eval 'bind '|SWL| '|<ButtonPress>| mousepress-cbproc)
      (swl:tcl-eval 'bind '|SWL| '|<ButtonRelease>| mouserelease-cbproc)
      (swl:tcl-eval 'bind '|SWL| '|<Double-ButtonPress>| (multi-mousepress-cbproc 2048))
      (swl:tcl-eval 'bind '|SWL| '|<Double-ButtonRelease>| (multi-mouserelease-cbproc 2048))
      (swl:tcl-eval 'bind '|SWL| '|<Triple-ButtonPress>| (multi-mousepress-cbproc 4096))
      (swl:tcl-eval 'bind '|SWL| '|<Triple-ButtonRelease>| (multi-mouserelease-cbproc 4096))
      (swl:tcl-eval 'bind '|SWL| '|<Motion>| mousemotion-cbproc)

      (set! swl:error-log-port
        (thread-make-parameter (swl:make-error-log-port)
          (lambda (x)
            (unless (output-port? x)
              (assertion-violationf 'swl:error-log-port "~s is not an output-port" x))
            x))))))

(define (swl:install-graphical-exception-handlers)
  (define (swl:open-new-window-to-show-error) #t)

  (import swl:macros)
  (import $system)
  (define +fixnum? (lambda (x) (and (fixnum? x) (fx> x 0))))

  (define lock-editor
    (lambda (bail-out)
      (lambda (e)
        (send e set-locked! #t))))

  (define unlock-editor
    (lambda (e)
      (send e set-locked! #f)))

  (define content-matching
    (lambda (error-sfd)
      (lambda (editor)
        (let* ([s (send editor get-buffer-content)]
              ; discard the dummy newline character Tk adds at end
               [len (fx- (string-length s) 1)])
          (and (= (source-file-descriptor-checksum error-sfd)
                  (source-file-descriptor-checksum
                    (make-source-file-descriptor ""
                      (let ([p (open-bytevector-input-port (string->utf8 s))])
                        (set-port-input-size! p len)
                        p))))
               editor)))))

  (define name-matching
    (lambda (filename)
      (lambda (editor)
        (let ([s (send editor get-filename)])
          (and (string? s)
               (let loop ([i (- (string-length filename) 1)]
                          [j (- (string-length s) 1)])
                 (or (fx< i 0) (fx< j 0)
                     (and (char=? (string-ref filename i) (string-ref s j))
                          (or (char=? (string-ref s j) #\/)
                              (loop (fx- i 1) (fx- j 1))))))
               editor)))))

  (define eof-error?
    (lambda (c)
      (define substring?
        (lambda (s1 s2)
          (and (<= (string-length s1) (string-length s2))
               (string=? s1 (substring s2 0 (string-length s1))))))
      (and (message-condition? c)
           (substring? "unexpected end-of-file" (condition-message c)))))

  (define show-error
    (lambda (ed c src start?)
      (let ([what (with-output-to-string (lambda () (display-condition c)))])
        (send ed show-sexpression-at (if (eof-error? c) 'start 'end) (source-object-bfp src) (source-object-efp src))
        (send ed show-explanation what))
      (send ed show)
#; ; skip while developing under KDE since there is a horrid delay...
      (send ed raise)
      (unlock-editor ed)))

  (define install-graphical-source-handler
    (lambda ()
      (let ([real-handler (base-exception-handler)])
        (base-exception-handler
          (lambda (c)
            (define (go src start?)
              (let ([editors (swl:get-editors)] [error-sfd (source-object-sfd src)])
                (for-each lock-editor editors)
                (let ([ed (ormap (content-matching error-sfd) editors)])
                  (for-each unlock-editor (remq ed editors))
                  (if ed
                      (show-error ed c src start?)
                      (let ([filename (source-file-descriptor-path error-sfd)])
                        (let ([match (ormap (name-matching filename) editors)])
                          (if match
                              (thread-fork
                                (lambda ()
                                  (send match show)
                                  (warning-dialog match
                                    (format
                                      "Not showing location of the exception within file ~s because the file appears to be open in an editor window whose content has been modified since the file was last loaded, or the file has been modified on disk by another editor."
                                      filename))))
                              (when (swl:open-new-window-to-show-error)
                                ; use zero instead of true source offset because
                                ; we just want to get the path here.
                                ; also were getting (values) on read-err33.ss
                                (with-values ($locate-source (source-object-sfd src) 0)
                                  (case-lambda
                                    [()
                                     (thread-fork
                                       (lambda ()
                                         (warning-dialog #f
                                           (format "Unable to show location of the exception within file ~s because no window is editing the file and the file either could not be found on disk or has been modified on disk since the file was last loaded."
                                             filename))))]
                                    [(path line char)
                                     (new-edit path
                                       (lambda (ed)
                                         (show-error ed c src start?)))]))))))))))
            (cond
              [($src-condition? c) (go ($src-condition-src c) ($src-condition-start c))]
              [(and (syntax-violation? c)
                 (let ([a (syntax->annotation (syntax-violation-form c))])
                   (and a (annotation-source a)))) =>
               (lambda (src) (go src #t))])
            (real-handler c))))))
    
  (install-graphical-source-handler)
  (void)
)

(module ()
(define swl:startup
  ;; it is an error to invoke this more than once (without "-" option)
  (lambda args
    (when (eq? (machine-type) 'i3nt)
      (let ([version-root (get-registry (format "HKEY_LOCAL_MACHINE\\SOFTWARE\\Chez Scheme\\csv~a\\VersionRoot" #%$scheme-version))])
        (when version-root
          (unless (getenv "SWL_ROOT")
            (putenv "SWL_ROOT" (format "~a\\swllib" version-root)))
          (unless (getenv "SWL_LIBRARY")
            (putenv "SWL_LIBRARY" (format "~a\\i3nt" version-root)))
          (unless (getenv "TCL_LIBRARY")
            (putenv "TCL_LIBRARY" (format "~a\\tcl8.5" version-root)))
          (unless (getenv "TK_LIBRARY")
            (putenv "TK_LIBRARY" (format "~a\\tk8.5" version-root))))))
    #;(printf "~a\n~a\n~a\n~a\n" (getenv "SWL_ROOT") (getenv "SWL_LIBRARY") (getenv "TCL_LIBRARY") (getenv "TK_LIBRARY"))

    (printf "Welcome to \"SWL\" Version ~a~n" swl:version)
    (printf "Please report \"SWL\" bugs to owaddell@cs.indiana.edu~n")
    (if (and (not (null? args)) (string=? (car args) "-"))
        (begin
          (fluid-let (#;
                      [require
                       (lambda (x . rest)
                         (printf "loading: ~s~n" x)
                         (load x))])
            (printf "Bypassing SWL initialization to support saving incremental heaps.~n")
            (printf "(No thread or graphics capabilities in this mode.)~n")
            (for-each load (cdr args))
            (new-cafe)))
        (let ((swl-dsp #f))
          (swl:load-foreign)
          (swl:foreign-init)

          (install-thread-system)

          (swl:init-preferences-hack) ; do this after thread system started

          (swl:init-io-subsystem)

          (swl:install-collect-request-handler)

          (swl:run-before-first-thread-fork)
        
          (set! swl:fallback-queue
            ;; this documentation won't be seen, (bug)
            (thread-make-parameter
              (swl:make-fallback-queue)
              (lambda (x) x)))
          
          (initialize-graphics-lib)

        ; must come before first peek-event or event-dispatch
          (when (memq (machine-type) '(ppcosx i3osx tppcosx ti3osx))
            (let ([op (open-output-string)])
              (display "set files_to_edit \"\"\n" op)
              (display "set generate_newedit_event 0\n" op)
              (display "proc ::tk::mac::OpenDocument {args} {\n" op)
              (display "  global files_to_edit\n" op)
              (display "  global generate_newedit_event\n" op)
              (display "  set files_to_edit \"$files_to_edit $args\"\n" op)
             ; if we could count on tk 8.5 we could use -data to pass along
             ; the files instead of using the files_to_edit "queue"
             ; see foreign.c s_eval comment regarding use of . as the window
              (display "  if ($generate_newedit_event) {\n" op)
              (display "    event generate \".\" <<NewEdit>>\n" op)
              (display "  }\n" op)
              (display "}\n" op)
              (let ([s (get-output-string op)])
                #;(printf "~a\n" s)
                (swl:raw-tcl-eval s)))
           ; should get our opendocument call up front if we were started
           ; by double click on filename(s) or drop of filenames on swl icon
            (swl:peek-event)
            (let ([ls (swl:parse-filenames
                        (let ([s (swl:tcl-eval 'set 'files_to_edit)])
                          (swl:tcl-eval 'set 'files_to_edit "")
                          s))])
             ; got files?  if so, pretend we were invoked with --edit switch
              (unless (null? ls) (set! args (cons "--edit" ls))))
            (swl:tcl-eval 'bind 'all '|<<NewEdit>>| new-edit-cbproc)
            (swl:tcl-eval 'bindtags "." '(all |SWL|))
          ; request NewEdit events for future double-clicks
            (swl:tcl-eval 'set 'generate_newedit_event 1))

          (set! thread-timer-interrupt-hook
            (lambda ()
              (when (and (not (eq? (thread-self) swl-dsp)) (swl:peek-event))
                (thread-reschedule swl-dsp (thread-highest-priority))
                (thread-yield))))
          
          ;; go into infinite loop servicing events
          (set! swl-dsp
            (thread-fork-group
             (lambda ()
               (thread-name "Event Dispatcher")
               (call/cc
                (lambda (reset-k) (thread-become-server! reset-k)))
               (let loop ()
                 (thread-yield)
                 (swl:event-dispatch)
                 (loop)))
             (thread-default-quantum)
             (thread-lowest-priority)
             ))

          (let ([app-q (thread-make-msg-queue 'app-q)])

            (define-record start-request (k queue) ([exit void]))
            (define-record end-request (token))

            (define fold
              (lambda (ls b fn)
                (if (null? ls) b (fn (car ls) (fold (cdr ls) b fn)))))

            (define swl:splash-screen
              (lambda ()
               ; Eliminated the code that used to let us click on the canvas to destroy it
               ; since Tk seems to have a strange bug wrt events delivered to widgets within
               ; toplevels that have override-redirect set to #t.
               ; Decided to length the delay to make the silly window more obtrusive.
                   (define wait (thread-make-msg-queue 'splash))
                   (define splash-delay 2000)
                   (define-class (<splash-canvas> p) (<canvas> p)
                     (ivars [parent p])
                     (inherited)
                     (inheritable)
                     (private)
                     (protected)
                     (public
                       [mouse-release (x y mods)
                        (on-error 'ignore (send parent destroy))]))
                  ; probably should complain if image file not found
                   (let* ([img (swl:with-library-file "splash.gif"
                                 (lambda (path)
                                   (create <photo> with (filename: path))))]
                          [iw (send img get-width)] [ih (send img get-height)]
                          [sp (create <toplevel> with (width: iw) (height: ih) (title: "SWL")
                                (override-redirect: #t)
                               ; Do not accept focus, so that window manager won't
                               ; misplace the focus when the window goes away (it was
                               ; stealing the focus from my repl --- very annoying when
                               ; you've already started typing.)
                                (accept-focus: #f)
                                (border-width: 0)
                                (background-color: 'black)
                                (geometry:
                                  (format "+~a+~a"
                                    (fxquotient (fx- (swl:screen-width) (send img get-width)) 2)
                                    (fxquotient (fx- (swl:screen-height) (send img get-height)) 2)))
                                (destroy-request-handler:
                                  (lambda (self)
                                    (thread-send-msg wait #f)
                                    #t)))]
                          [splash-can (create <splash-canvas> sp with
                                        (accept-focus: #f)
                                        (width: iw)
                                        (height: ih)
                                        (border-width: 0))])
                     (create <canvas-image> splash-can 0 0 with (anchor: 'nw) (image: img))
                     (let ([fnt (create <font> 'times 18 '(bold))])
                       (create <canvas-text> splash-can 90 170 with     ; or 55 170   to left align
                         (title:
                          (format "~aChez Scheme Version ~a"
                            (if (system-ref '$compiler-is-loaded?)
                                ""
                                "Petite ")
                            (system-ref '$scheme-version)))
                         (anchor: 'nw)
                         (fill-color: 'red)
                         (font: fnt)))
                     (let ([fnt (create <font> 'helvetica 14 '())])
                       (create <canvas-text> splash-can 90 210 with     ; or 55 210
                         (title: (format "SWL Version ~a" swl:version))
                         (anchor: 'nw)
                         (font: fnt)))
                     (thread-fork
                       (lambda ()
                         (show splash-can)
                         (show sp)
                         (send sp raise)
                         (thread-sleep splash-delay)
                         (on-error 'ignore (send sp destroy))))
                     (thread-receive-msg wait))))

               (define build-update-menu-parameter
                ; Note: it's important to have the (swl:make-application proc) below
                ;       or else REPLs forked from the menu have bum parameters and their
                ;       begin-exp mark gets out of sync (? wrong waiter-prompt-and-read).
                 (lambda (who menu-title default-items)
                   (let ((items '()) (cascades '()))
                     (define-swl-class (<collected-cascade>) (<cascade-menu-item>)
                       (ivars)
                       (inherited)
                       (inheritable)
                       (private)
                       (protected)
                       (public
                         [init ignore-args
                          (critical-section
                            (set! cascades (cons self cascades)))]
                         [destroy ()
                          (critical-section
                            (send-base self destroy)
                            (set! cascades (remq self cascades)))]))
                     (define build-item
                       (lambda (item)
                         (let ([title (car item)] [proc (cdr item)])
                           (create <command-menu-item> with
                             (title: title)
                             (action: (lambda (i) (thread-fork-group proc)))))))
                     (define build-default-items
                       (lambda ()
                         (let ([ls (map build-item default-items)])
                           (if (null? ls)
                               ls
                               (cons (create <separator-menu-item>) ls)))))
                     (define build-menu
                       (lambda ()
                         (let f ([ls items] [mis (build-default-items)])
                           (if (null? ls)
                               (create <menu> mis)
                               (f (cdr ls) (cons (build-item (car ls)) mis))))))
                     (define title-follows?
                       (lambda (x y)
                         (string-ci>?
                           (if (pair? x) (car x) x)
                           (if (pair? y) (car y) y))))
                     (case-lambda
                       [()
                        (create <collected-cascade> with (title: menu-title) (menu: (build-menu)))]
                       [(title proc)
                        (unless (or (string? title)
                                    (and (pair? title)
                                         (string? (car title))
                                         (string? (cdr title))))
                          (assertion-violationf who "~s is not a valid title for a help menu item" title))
                        (unless (or (not proc) (procedure? proc))
                          (assertion-violationf who "~s is not #f or a procedure" proc))
                        (let ([hit (assoc title items)])
                          (if proc
                              (if hit
                                  (set-cdr! hit proc)
                                  (set! items
                                    (sort title-follows? (cons (cons title proc) items))))
                              (when hit (set! items (remq hit items)))))
                        (for-each
                          (lambda (i) (set-menu! i (build-menu)))
                          cascades)]))))

           ; The app-q and servicing thread give us a way to have the SWL
           ; process exit when the last registered application exits.
           ; When the list of application instances is empty the SWL process
           ; exits.
           ;
           ; Applications register via swl:begin-application and unregister
           ; via swl:end-application.  Applications register an exit-notify
           ; thunk that allows them to respond to "Exit SWL" or (abort).
           ; The editor uses this to prompt for saving unsaved buffers.
           ; The graphical error log port (swl:error-log-port) uses this to
           ; exit only when the user clicks on the "dismiss" button.
           ; The server.ss code uses this to keep SWL open until we've finished
           ; processing the command line arguments.
            (define service-make-app-queue
              (let ([insts '()])
                (define run-exit
                  (lambda (req)
                    ((start-request-exit req))))
                (lambda ()
                  (let ([msg (thread-receive-msg app-q)])
                    (cond
                      [(start-request? msg)
                       (let ([token (string #\t)])
                         (set! insts (cons (cons token msg) insts))
                         (thread-fork-group
                           (lambda ()
                             (let ([proc
                                    (parameterize ([swl:fallback-queue (start-request-queue msg)])
                                      ((start-request-k msg) token))])
                               (unless (procedure? proc)
                                 (assertion-violationf 'swl:begin-application "~s is not a procedure" proc))
                               (set-start-request-exit! msg proc)))))]
                      [(end-request? msg)
                       (let ([token (end-request-token msg)])
                         (if (eq? token 'exit-all)
                             (thread-fork
                               (lambda ()
                                 (for-each run-exit (map cdr insts))))
                             (set! insts
                               (let rem ([insts insts])
                                 (cond
                                   [(null? insts) '()]
                                   [(eq? token (caar insts))
                                    (let ([req (cdar insts)])
                                      (thread-send-msg (swl:fbq-queue (start-request-queue req)) 'exit)
                                      (thread-fork (lambda () (run-exit req))))
                                    (cdr insts)]
                                   [else (cons (car insts) (rem (cdr insts)))])))))]))
                  (unless (null? insts) (service-make-app-queue)))))

             (swl:api-procedure set! swl:application-menu
               ;* \formdef{swl:application-menu}{procedure}{(swl:application-menu)}
               ;* \returns {an instance of \scheme{<cascade-menu-item>}}
               ;* \formdef{swl:application-menu}{procedure}{(swl:application-menu title thunk)}
               ;* \ret{unspecified}
               ;* When invoked with no arguments, \scheme{swl:application-menu}
               ;* returns a cascade menu item for use as a top level entry
               ;* in the menu of a \scheme{<toplevel>} (\emph{i.e.} supplied as the
               ;* \scheme{menu:} attribute of the \scheme{<toplevel>} or installed
               ;* via its \scheme{set-menu!} method).
               ;* 
               ;* When invoked with two arguments, \scheme{swl:application-menu} expects
               ;* \scheme{title} to be a valid \scheme{<command-menu-item>} title
               ;* (except that images are not allowed due to incompatibilities with
               ;* Windows platforms) and \scheme{thunk} to be either a procedure of
               ;* no arguments, to be invoked when this menu item is chosen, or \scheme{#f}.
               ;* If \scheme{thunk} is a procedure, then existing and future SWL application
               ;* menus will contain an entry with the given title that invokes this
               ;* procedure.  If \scheme{thunk} is \scheme{#f}, the corresponding menu
               ;* entry (if any) is removed from all SWL application menus.
               ;* Items on the SWL application menu are sorted alphabetically, except
               ;* the last two items:  one titled ``Version...'' that displays the
               ;* version information on the initial splash screen and one titled
               ;* ``Exit SWL'' that does what you might expect.
               ;* These default menu entries cannot be removed.
               (build-update-menu-parameter 'swl:application-menu "S_WL"
                 (list (cons "E_xit SWL" (lambda () (swl:end-application 'exit-all))))))

             (swl:api-procedure set! swl:help-menu
               ;* \formdef{swl:help-menu}{procedure}{(swl:help-menu)}
               ;* \returns {an instance of \scheme{<cascade-menu-item>}}
               ;* \formdef{swl:help-menu}{procedure}{(swl:help-menu title thunk)}
               ;* \ret{unspecified}
               ;* When invoked with no arguments, \scheme{swl:help-menu}
               ;* returns a cascade menu item for use as a top level entry
               ;* in the menu of a \scheme{<toplevel>} (\emph{i.e.} supplied as the
               ;* \scheme{menu:} attribute of the \scheme{<toplevel>} or installed
               ;* via its \scheme{set-menu!} method).
               ;* 
               ;* When invoked with two arguments, \scheme{swl:help-menu} expects
               ;* \scheme{title} to be a valid \scheme{<command-menu-item>} title
               ;* (except that images are not allowed due to incompatibilities with
               ;* Windows platforms) and \scheme{thunk} to be either a procedure of
               ;* no arguments, to be invoked when this menu item is chosen, or \scheme{#f}.
               ;* If \scheme{thunk} is a procedure, then existing and future SWL help
               ;* menus will contain an entry with the given title that invokes this
               ;* procedure.  If \scheme{thunk} is \scheme{#f}, the corresponding menu
               ;* entry (if any) is removed from all SWL help menus.
               ;* Items on the SWL help menu are sorted alphabetically, except that the
               ;* last item is always a ``Documentation'' button that displays the
               ;* online documentation.
               ;* The default menu entry cannot be removed.
               (build-update-menu-parameter 'swl:help-menu "_Help"
                 (list 
                       (cons "_About SWL"
                             (lambda ()
                               (swl:with-library-file
                                 "about.html"
                                 new-viewer)))
                       (cons "_Documentation"
                             (lambda ()
                               (swl:with-library-file
                                 "index.html"
                                 new-viewer)))
                       (cons "_Version..." (lambda () (swl:splash-screen))))))

             (swl:api-procedure set! swl:begin-application
               ;* \formdef{swl:begin-application}{procedure}{(swl:begin-application \var{start-k})}\label{swl:begin-application}
               ;* \ret{unspecified}
               ;* Installs a new fallback queue and service thread and
               ;* calls \scheme{start-k} to construct the application.
               ;* \scheme{start-k} is passed a unique application token
               ;* that may be supplied to \scheme{swl:end-application}
               ;* in order to remove the applicaiton from the list of
               ;* registered applications.
               ;* SWL exits when the last registered application is removed.
               ;* The \scheme{start-k} is expected to return an exit-notify
               ;* thunk.
               ;* The exit-notify thunk is invoked when the user clicks on
               ;* the SWL ``Exit SWL'' button and by the default
               ;* \scheme{abort-handler}.
               ;*
               ;* Widgets created during the evaluation of \scheme{start-k}
               ;* share the new fallback event queue.
               ;* Because events must be processed serially,
               ;* using a single event queue for all applications
               ;* would allow one ill-behaved application to
               ;* prevent other applications in the Scheme session
               ;* from receiving events.
               (lambda (start-k)
#;(parameterize ([print-graph #t])
  (pretty-print `(swl:begin-application ,((((inspect/object start-k) 'code) 'source) 'value)) #%$console-output-port))
                 (thread-send-msg app-q
                   (make-start-request start-k
                     (swl:make-fallback-queue)))))

             (swl:api-procedure set! swl:end-application
               ;* \formdef{swl:end-application}{procedure}{(swl:end-application \var{token})}
               ;* \ret{unspecified}
               ;* Remove from the list
               ;* of active applications the instance granted this
               ;* token and invoke the exit thunk supplied at that
               ;* time (see \ref{swl:begin-application}).
               ;* If \var{token} is the symbol \scheme{exit-all}, invoke the
               ;* remaining exit thunks and exit SWL if no protected
               ;* applications remain.
               (lambda (token)
                 (thread-send-msg app-q (make-end-request token))))

            ; moved this here from repl.ss (etc) for saved heap
             (swl:application-menu "New _Editor" new-edit)
             (swl:application-menu "New _REPL" new-repl)

             (swl:help-menu "for _Editor"
               (lambda () (swl:with-library-file "edit.html" new-viewer)))
                          
             (swl:help-menu "for _REPL"
               (lambda () (swl:with-library-file "repl.html" new-viewer)))

             ;; Send errors and warnings to the graphical notification window.
             ;; Have to do this after the above have been exported.
             (console-error-port (swl:error-log-port))
             (console-output-port (swl:error-log-port))

           ;; This is a good place to run code that we want to have
           ;; available before loading command-line arguments and
           ;; before starting the repl
           ;;  - for example, this could load preference and eval
           ;;  - it could install the graphical source-warning and
           ;;    source-error handlers so they'd be used while loading
           ;;    args
           ;;  - it could install a new edit widget or new repl widget
           ;;    before we fire off the first one
             (swl:run-before-args)

             (swl:install-graphical-exception-handlers)

             (let ([e (copy-environment (interaction-environment))])
               (let ([clean-environment
                      (case-lambda
                        [() (clean-environment 'shared)]
                        [(who)
                         (unified-interaction-environment
                           (case who
                             [(own) #f]
                             [(shared) #t]
                             [else
                              (assertion-violationf 'clean-environment
                                "expected one of the symbols 'own or 'shared, but got '~s instead" who)]))
                         (interaction-environment (copy-environment e))])])
                 (set-top-level-value! 'clean-environment clean-environment e)
                 (set-top-level-value! 'clean-environment clean-environment)))

             (let () ; expedient let
  
               (define do-command-line
                 (lambda (args)
                   (if (and (not (null? args))
                            (string=? (car args) "--edit"))
                       (if (null? (cdr args))
                           (new-edit)
                           (let* ([fn (cadr args)] [upfn (path-parent fn)])
                             (unless (string=? upfn "")
                               (current-directory upfn))
                             (for-each new-edit (cdr args))))
                       (begin (for-each load args) (new-repl 'default)))))
  
             (let ()
               (include "../swl/server.ss")

               (start-swl-server args))
  
             (let ([gprefs (swl:load-preferences 'swl-global)])
  
               (define warn-prefs
                 (lambda (msg)
                   (swl:begin-application
                     (lambda (token)
                       (warning-dialog 'noblock
                         (string-append "SWL global preferences section malformed\n\n" msg))
                       (swl:end-application token)
                       (lambda () (swl:end-application token))))))
  
              ; leave real abort handler in effect for start-swl-server
               (abort-handler
                 (lambda ()
                   (swl:end-application 'exit-all)))
  
              ; show the splash screen?
               (when (cond [(assq 'splash-screen gprefs) => cdr] [else #t])
                 (swl:splash-screen))

             ; change directory?
               (let ([dir (cond [(assq 'start-dir gprefs) => cdr] [else #t])])
                 (when (and (string? dir) (file-exists? dir))  (cd dir)))

             ; loading prefs first so we don't have partially formed
             ; repl window showing up before warnings...
             ; Also lets us decide whether or not to have the splash screen.
               (thread-fork
                 (lambda ()
                   ((on-error (with-message msg (lambda () (warn-prefs msg)))
                     ; Evaluate startup expressions.
                     ; This is a potential security hole.
                     ; If no on-startup specified, then start a repl.
                      (cond
                        [(assq 'on-startup gprefs) =>
                         ; previously had been setting up special error handlers
                         ; while running the startup code, but realized that was
                         ; a bad idea since you might use this to do things like
                         ; start up three repls and two editors that you expect
                         ; to work normally --- but that would inherit the funny
                         ; exception-handlers and give unexpected results.
                         (lambda (x)
                           (on-error
                             (with-message msg
                               (warn-prefs
                                 (string-append "Error occurred while compiling on-startup expressions from preferences file.\n\n" msg)))
                             (interpret `(lambda (args) ,@(cdr x)))))]
                        [else do-command-line]))
                  args))))
               ) ; end expedient let
  
               (service-make-app-queue))
          (swl:tcl-finalize)))))
(scheme-start swl:startup))

)

#!eof      ------------------------- EOF -------------------------------

I believe we can get all this functionality back via a modest patch file
since the various menus all go through new-repl or new-edit to create the
widgets and we may be able to derive subclasses from those classes to fix
most of what we need to fix.

(eval-when (compile) (import swl:internal))

;-------
; DESIGN
;-------
; given who, msg, source, compute new position and new message
; also pass in start and end point for the markup
;  maybe if the end point isn't there, have the widget compute it for us
;  that may turn out to be more convenient for the single stepper
;  - may want to be able to cache a table of token start/end points in
;    the widget
;  ? maybe decide where to put the cursor based on the kind of error
;    but always highlight the range given to us by the (new) reader

;------------------
; PATTERNS TO CATCH   ? maybe a pattern language over tokens
;------------------
; dot rparen       ; "unexpected close parenthesis" (patched in cpdebug workarea
;                  ; to say "expected one item after dot")
; dot token token  ; more than one item found after
; dot dot          ; unexpected dot
;
; * Need to do something to handle unexpected end-of-file errors
;    - but this needs to be very careful wrt unterminated string constants
;       - suspect funny business if multiline string, esp if it contains
;         parentheses, maybe?
;       - maybe we can handle this with a web-based pretty-printer that
;         shows their code with syntax highlighting that shows string
;         constants in a different color (or tell them to use emacs?)

;------------------------
; Error messages to parse
;------------------------
;
; ===> desperately need read to report bfp instead of fp in error messages
;      ? why is the answer always "go hack the Chez Scheme source" ?
;
;  maybe leave it up to individual error guys as to which pos they report
;   ? maybe only give start source when they click the Where? button
;
;  obstacles:  ; these use xdefine instead of define-state
;    rd-make-number-or-symbol   ; ok for bxcall
;    rd-eof-error               ; ok for bxcall
;    rd-fix-graph               ; ok for bxcall
;    rd-mark                    ; ok for bxcall
;    rd-fill-vector             ; ok for bxcall, but do we want bfp?
;                               ; if too many elts, maybe show elt ?
;  maybe add a bxcall / bxdefine that pass bfp for us
;    ? does bxmvlet make any sense?
;
;  * At times like these, I really want an analysis that can hand me the
;    transitive closure of all the places that would be affected by source
;    change.
;
; "duplicate mark #~s= seen"
;   --> new read gives source for start of first mark up through end
;       so basically hilights a lot of read-err1.ss
; "expected close brace terminating gensym syntax"
; "expected one item after dot (.)"
;    this is one I added
; "fasl object created by different release"
; "invalid character #\\..."
; "invalid character name"
; "invalid hex string character value"
; "invalid number syntax"
; "invalid sharp-sign prefix"
;    can have # or #% or #n%
; "invalid string character \x" pos points to character just after,
;    so pass in pos-2 and pos as start and end
;     * maybe reader should always return bfp unmodified,
;       this may just be a place where SWL has to backparse (easy to scan
;       back til it finds the first backslash in this case)
; "invalid syntax #!..."
; "invalid vector length"
; "mark #~s= missing"
;   --> new read gives source start/end for basically the whole
;       darn sexpression instead of just giving source for the mark
; "more than one item found after dot (.)"
; "non-symbol found after #["
; "outdated object file format"
; "too many vector elements supplied"
; "unexpected close X"
;    X is one of parenthesis, bracket
; "unexpected dot"
; "unexpected end-of-file"
; "unexpected end-of-file reading X"
;    X is one of string, symbol, # prefix, gensym, block comment, character
;   --> for these, we want to set cursor position to the start of the token
;    - start pos good for string, # prefix
;       * note that w/ multiple strings, system may think last string is
;         the one that doesn't match, when in fact it may be that some
;         earlier string constant was not terminated
;          ---> would definitely help to be able to mark the perceived
;               string boundaries
;                - could we get the system to do bouncing for double quotes?
;    - start pos WRONG for symbol, gensym
; "unrecognized record name ~s"
; "wrong number of fields supplied for record ~s"

; SKIPPED: "unresolvable cycle constructing record of type"
; SKIPPED: "unexpected internal token type"

; maybe have some menu button they can click like "Why?"
; that's normally hidden, but shows up when we have some
; additional info

; may be some code like this in repl-text.ss 

; use on-error macro ?

(define-record tinfo (type start))

; note that read-token can bug out to error, eg. while reading gensym
; or while reading string.
; appears that 6.9 doesn't return symbol-brace, but keeps reading the gensym

(define locate-token-start
  (lambda (ip target)
    (on-error #f
      (file-position ip 0)
      (let loop ()
        (let-values ([(type token start end) (read-token ip)])
          (if (= end target)
              start
              (and (< end target) (not (eq? type 'eof)) (loop))))))))

(define locate-parse-error
 ; return the file offset of the character where the reader will report
 ; an error if we attempt to read, or return #f
  (lambda (ip)
    (define last #f)
    (define match?
      (lambda (left right)
       ; cp1in can't unroll memq if we push case inside
        (case right
          [(rparen) (memq left '(lparen vparen vnparen vfxparen vfxnparen))]
          [(rbrack) (memq left '(lbrack record-brack))]
          [(rbrace) (memq left '(lbrace symbol-brace))]
          [else #f])))
    (define parse
      (lambda ()
        (let loop ([stack '()])
          (let-values ([(type token start end) (read-token ip)])
            (case type
              [(eof) (and (not (null? stack)) (tinfo-start (car stack)))]
              [(lparen lbrack vparen vnparen vfxparen vfxnparen lbrace record-brack)
               (loop (cons (make-tinfo type start) stack))]
              [(rparen rbrack rbrace)
               (cond
                 [(null? stack) #f]
                 [(match? (tinfo-type (car stack)) type) (loop (cdr stack))]
                 [else (tinfo-start (car stack))])]
              [else (loop stack)])))))
    (define handle-read-token-error
      (lambda ()
        (on-error #f
          (file-position ip last)
          (let skip ()
            (let ([c (peek-char ip)])
              (and (not (eof-object? c))
                   (if (char-whitespace? c)
                       (skip)
                       (case c
                         [(#\;)
                          (let loop ()
                            (read-char ip)
                            (case (peek-char ip)
                              [(#!eof #\newline) (skip)]
                              [else (loop)]))]
                         [(#\#)
                          (let ([here (file-position ip)])
                            (read-char ip)
                            (case (peek-char ip)
                              [(#\;) (read-char ip) (or (parse) (skip))]
                              [(#\|)
                               (read-char ip)
                               (let skip-block-comment ([stack (list here)])
                                 (if (null? stack)
                                     (skip)
                                     (case (peek-char ip)
                                       [(#\|)
                                        (read-char ip)
                                        (case (peek-char ip)
                                          [(#\#)
                                           (read-char ip)
                                           (let ([stack (cdr stack)])
                                             (if (null? stack)
                                                 (skip)
                                                 (skip-block-comment stack)))]
                                          [else (skip-block-comment n)])]
                                       [(#\#)
                                        (let ([here (file-position ip)])
                                          (read-char ip)
                                          (case (peek-char ip)
                                            [(#\|)
                                             (read-char ip)
                                             (skip-block-comment
                                               (cons here stack))]
                                            [else
                                             (skip-block-comment stack)]))]
                                       [(#!eof) (car stack)])))]
                              [else here]))]
                         [else (file-position ip)]))))))))
    (on-error (handle-read-token-error)
      (parse))))

) ; end module (swl:startup)


;----------------------
; register-interest affects
;  - destroy method
;  - ability to modify file
#;  ; let's use the request stuff for single-stepper and skip it here
    ; (probably should be integrated into the lock mechanism, btw)
(let ([self (thread-self)])  ;; IMPORTANT: grab thread out here since notify
  (send e register-request   ;; may be run in editor's thread
    (make-request "to show the code that generated a warning or an error"
      (lambda () (thread-kill self)))))
;----------------------

(define-record request (reason denied))


#!eof

(load "/tmp/read.patch")
(cd "tests/error-help")

(begin
  (define-record f800 (a b))
  (record-reader "zinjanthropus" (type-descriptor f800)))
(begin
  (define-record $acyclic ((immutable notme)))
  (record-reader '$acyclic (type-descriptor $acyclic)))
(define n 1)
(define stem "read-err")
(define stem "test")
(define (go)
  (let ([fn (format "~a~a.ss" stem n)])
    (printf "n=~s\n" n)
    (when (file-exists? fn) (set! n (+ n 1)) (load fn))))


(define get-range
  (lambda (file start end)
    (let ([ip (open-input-file file)])
      (file-position ip start)
      (let ([len (- end start)])
        (let ([s (make-string len)])
          (block-read ip s len)
          (close-input-port ip)
          s)))))

