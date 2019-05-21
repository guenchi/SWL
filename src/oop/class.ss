;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

; module grafted on after the fact to tidy things up a bit
;
; [reverted] changed (define residual ...) to (ctdef residual ...)
; to help get this bootstrapped despite local definitions.
;  aha! but using ctdef screws us up with classes that contain
;  free references to local variables
; *** wait: use internal-defines-as-letrec* instead

(eval-when (compile eval) (internal-defines-as-letrec* #t))

(module swl:oop (class?
                 instance?
                 (make <base> <root> <bootstrap>)
                 send
                 send-apply
                 (define-class
                   ctdef
                   <root>
                   <base>
                   class?
                   instance?
                   <bootstrap>
                   swl:id-list-index
                   swl:id-filter
                   swl:id-intersect
                   swl:id-setrember
                   swl:make-class
                   swl:dispatch-table
                   class-dispatch-table
                   safe/unsafe
                   issue-syntax-error-msgs
                   swl:id-subset?
                   swl:build-application)
                 swl:dispatch-table   ; sigh, currently used by src/swl/base2.ss
                 <base>)

(import scheme)

(include "defint.ss")
(include "dispatch.ss")

(define-syntax ctdef ; compile-time definitions  (rough meta-module support)
  ; use the #%$ versions to bypass (interaction-environment)
  (lambda (x)
    (syntax-case x ()
      [(_ x e)
       (let ([genny (gensym)])
         (with-syntax ([genny (datum->syntax-object #'x genny)])
           #'(define-syntax x
               (begin
                 (eval-when (compile load eval)
                   (#%$set-top-level-value! 'genny (rec x e)))
                 (identifier-syntax (#%$top-level-value 'genny))))))])))

(ctdef swl:id-list-index
  (lambda (id ils)
    (let lp ((ils ils) (i 0))
      (syntax-case ils ()
        [() #f]
        [(x . more)
         (if (literal-identifier=? #'x id)
             i
             (lp #'more (fx+ i 1)))]))))

(ctdef swl:id-filter
  (lambda (fn)
    (lambda (is1 is2)
      (let lp ((is2 is2))
        (if (null? is2)
            '()
            (if (fn is1 is2)
                (cons (car is2) (lp (cdr is2)))
                (lp (cdr is2))))))))

(ctdef swl:id-intersect (swl:id-filter (lambda (is1 is2) (swl:id-list-index (car is2) is1))))

(ctdef swl:id-setrember (swl:id-filter (lambda (is1 is2) (not (swl:id-list-index (car is2) is1)))))

;(define swl:id-union
;  (lambda (is1 is2)
;    (let lp ((is2 is2))
;      (if (null? is2)
;          is1
;          (if (not (swl:id-list-index (car is2) is1))
;              (cons (car is2) (lp (cdr is2)))
;              (lp (cdr is2)))))))

(ctdef swl:id-subset?
  (lambda (is1 is2)
    (andmap (lambda (x) (swl:id-list-index x is2)) is1)))

(define-syntax issue-syntax-error-msgs
  (syntax-rules ()
    ((_ (test . body) ...)
     (when (or (let ((x test)) (when x . body) x) ...)
       (assertion-violationf 'define-class "syntax errors found")))))

;; NOTE:
;;   * I just reviewed send-base implementation for public and protected and
;;     it's fine.  What's broken is that send-base macro should be defined
;;     within the scope of the whole class definition (not just the methods).
;;     (see mail to wally)
;;        - consequently I'm taking out the external send-base hack I'd added
;;          long ago for the <flex-text> class.
;;
;; BUG fix
;;   - to ensure that protected and private methods only receive the correct
;;     ivars structure (that passed in to the public method), we implicitly
;;     pass "self".  Note that self binding is not mutable (structure is, but
;;     not the binding, thanks to local set! macro) so this is safe.
;;       * incompatibility *   protected and private method calls no longer
;;         permit explicit self argument to be passed (even in send-base
;;         on protected).
;;   - to ensure that private and protected methods are only passed the correct
;;     instance, and to prevent confusion with public methods (which are really
;;     labels in a case statement), we disallow method references in any but
;;     call (or send) contexts.
;;       - when priv/prot methods are referenced for value, we could wrap them
;;         with  (lambda args (apply method-name self args))
;;         assuming the compiler generates good code for this
;;   - to emphasize the distinction between public (entry) methods and
;;     private / protected (internal) methods, we now disallow send syntax
;;     for private and protected guys.  This isn't totally satisfactory
;;     since we still have to support send-base for protected methods,
;;     and we lose some symmetry.
;;   - TODO:  see about moving the fluid-let-syntax'es around for faster expand

;; Document:
;;   - change (self an implicit arg) in method call syntax for prot / priv
;;   - send-base only makes sense for prot / public  (prot has self implicit)
;;   - methods can only be referenced in a call context


;;  Recent (somewhat bogus) hack:
;;    we now allow rest args in the class arguments
;;    I threw in something called swl:build-application
;;    thinking at the time it might serve a useful purpose
;;    but it can probably be safely removed.

#;(set! swl:define-class-verbose #f)

;; This version may still be hosed.
;; Send's to methods that are private or protected names of the class
;; are rewritten as direct calls or indirect calls (respectively) rather
;; than the generic send.  This is only done when the instance we're sending
;; to is self.  The self binding is made immutable to make this (more likely
;; to be) valid.  I still haven't thought it through in great detail.
;; It will be hosed if someone let-binds self for some reason and then
;; tries to call a private or protected method.

;(define show? #f)
;(define-syntax syntax-trace
;  (lambda (form)
;    (syntax-case form ()
;      ((_ e)
;       show?
;       (syntax (begin (pretty-print 'e) e)))
;      ((_ e)
;       (syntax e)))))

;; simple class system based on object-lambda dispatcher
;; but with ivars and inheritance

;(printf "setting current-expand sc-expand~n")
;(current-expand
;  (lambda (x)
;    (parameterize ([current-eval interpret])
;      (sc-expand x))))

;; TODO     Todo      todo
;;   * BUG:  cyclic constants hosed when present in the ivars initializers
;;   - May want to implement a send-base for protected methods?
;;   - See comments below on benefits of revising the macros so that
;;     build-subclass builds a macro that knows everything it needed
;;     to know all the way up to base.  Should be a pretty minor variant
;;     of what I've got now.  Will help avoid recomputation (making expansion
;;     faster) and will make it safe to redefine superclasses.
;;   - add regression test for visibility of self in various
;;     categories of methods and mu-lambda methods
;;   - bug:  if the identifier ... is free (or quoted) in a method
;;     definition we get an extra elipsis error
;;   - things that are missing:
;;       * need a way to have local variables for a class or method.
;;         eg.  (let ((legal-options (list x y z d)))
;;                (method ...))
;;       * a way to define classes that take variable numbers of make formals
;;       * would be cool to have a way to totally drop an instance
;;       * macro nature of things means that we can't use something like
;;         (isa? x <bar-class>) in the definition of foo-class when
;;         bar-class inherits from foo-class  (have to eta-expand.)
;;   - need to do some cleanup.  there is some vestigial code left
;;   - decide what to do about non-call occurrences of method names
;;   - should check to make sure they don't have same priv prot and pub names.
;;   - right now you can have methods and instance vars of the same name.  change?
;;   - must be careful with local class definitions:  wrap let around some class defns
;;     then try making an instance.  it dies trying to get the class-dispatch-table because
;;     the class var is still bound to void. 
;;     * basically it just violates a rule of letrec when you try to inherit from a
;;       class defined in the same scope  (need to have another let there)
;;   - fix error messages?  currently get unknown method error instead of
;;     wrong number of args error.
;;   - test the hell out of it
;;       - test hygienicity (make sure uses of fluid-let-syntax are reasonable)
;;   - perhaps redo the whole shooting match using procedures
;;   ? can we avoid using a slot in each instance (for the table of protecteds)
;;     when there are none?  the idea would be to make the class obtainable by
;;     a special public method in order to keep instances for things like pr
;;     small
;;   - eventually, expand into a begin with definitions for
;;       - privates
;;       - publics
;;       - dispatch table (can then (since no 1st class classes) expand dispatch tables for
;;         subclasses to include the clauses of the parent appended at the end of the clauses
;;         for the subclass) --- swl:dispatch-table can weed out duplicates
;;     when we do this maybe we'll be able to get rid of the two separate fluid-let-syntax guys
;;   - modify macro system to support renaming of things introduced in definitions like
;;      (begin (define x 12) (define foo (lambda () x))) so that any defn introduced at this
;;     stage is visible only to the other stuff introduced here (think chi-body)
;;   - make a version of gensym that sets the gensym-prefix to be a concise representation
;;     of the time of day, the internet address, etc.  should be able to get a bunch of stuff
;;     just once per scheme session
;;     * but these things need to be interned
;;   - figure out what to do about references to method names in non-call posn
;;     This will be easy to handle once we go to the "lifted" representation:
;;     it'll happen for free without id macros.
;;
;; fixed?
;;   - need to come up with convenient syntax for class
;;   - need to limit scope of the fluid-let-syntax?


;; NOTES
;;
;;   The (let-syntax ((self (identifier-syntax self))) ...) around the whole mess
;;   is intended to restrict the scope of the later fluid-let-syntax.  The trick
;;   of making it identifier-syntax for a reference to self ensures that both
;;   the references to self introduced by the macro and those present in the
;;   input are captured by the fluid-let-syntax binding.
;;
;;   This worked great until I split it into two macros.
;;
;;   It looks really gross to have two separate fluid-let-syntax guys
;;   for the _priv-names (one inside the method bodies we letrec to the
;;   privtmps, and one inside the body of the letrec).  Unfortunately
;;   it doesn't seem to work to abstract the fluid-let-syntax out into a macro
;;   because the macro needs to be done at the same time as the letrec.
;;   Once we go to infernal define it may work out.
;;
;;   Important to propagate the protecteds up the way I'm doing it now.
;;   Using union may not preserve order (doesn't make it as apparent that
;;   we need to).  If we don't remove the duplicates then we can end up
;;   with some nasty bugs.

;; ivars        lists the instance variables used by the class
;; inherited    subset of the ivars that are inherited from base class
;; inheritable  subset of the ivars that may be inherited by subclasses
;; private      methods local to this class, not inherited nor inheritable
;; protected    methods visible only in this class and subclasses
;; public       methods visible everywhere

#| SYNTAX
(define-class (foo a b c) (bar a (+ b c))
  (ivars (b (* a b)) (x c) (y (+ a b)))     ;; should ivars and initializers be disjoint?
  (inherited a c z)
  (inheritable a b c)
  (private
    [spam (x y) (+ x y)]
    [quack (z) (list z)])
  (protected
    [moo (x) x])
  (public
    [bar (x y) (cons x y)]
    [foo (x y) (cons x y)]
    [cow (x y) (cons x y)]))
|#

; Instance
;       ------------------------------------
;       |    | class | ivar |  ...  | ivar |
;       --|------|--------------------------
;         |      |
;      dispatch  --------------------------------------------------------
;        code    | dispatch code | base-class | method |  ...  | method |  (protected methods)
;                ----------------------|---------------------------------
;                                      |
;                                  dispatch code etc.
;
; (instance caches the dispatch code of its class)
;
; For now first field of instance is a closure, eventually it's just
; a code pointer.  Have to do something with any free vars it might
; have though...

;; this is 2 even though the picture above makes it look like 1 because
;; there's a class tag at the beginning of the class vector

(define-syntax base-class-dispatch-table-offset (identifier-syntax 2))

;; avoid apply if possible
(define-syntax swl:build-application
  (lambda (form)
    (syntax-case form ()
      ((_ rator rand ...)
       (syntax (rator rand ...)))
;; Not used?
#;    ((_ rator . rands)
       (with-syntax ((rands (flatten-args (syntax rands))))
         (syntax (apply rator . rands)))))))

(define-integrable class?
  (lambda (x)
    (and (vector? x)
         (fx>= (vector-length x) 2)
         (eq? (vector-ref x 0) 'class))))

(define-syntax swl:make-class
  (syntax-rules ()
    ((_ dispatch-table prot ...)
     (vector 'class dispatch-table prot ...))))

(define-integrable class-dispatch-table
  (lambda (class)
    (safe/unsafe
      (if (class? class)
          (vector-ref class 1)
          (assertion-violationf 'class-dispatch-table "~s is not a class" class))
      (vector-ref class 1))))

; We can't do the following because the putprop implied in define-class
; would be global and a local scope might have a non-class binding that
; shadowed an outer class binding.  This would let us expand into a
; bogus but unchecked make in the local scope.  We really need a way to
; manipulate the compile-time environment.
;
;(define-syntax make
;  (lambda (form)
;    (syntax-case form ()
;      [(_ class arg ...)
;       (let ((cls (syntax-object->datum (syntax class))))
;         (if (and (symbol? cls) (getprop cls '*swl-class?*))
;             (syntax ((class 'make-instance class ()) arg ...))
;             (syntax (assertion-violationf #f "attempt to instantiate non-class ~s" class))))])))

(define-syntax make
  (lambda (form)
    (syntax-case form ()
      [(_ class arg ...)
       (syntax
         (begin
           (safe/unsafe
             (unless (class? class)
               (assertion-violationf #f "attempt to instantiate non-class ~s" class))
             (void))
           ((class 'make-instance class ()) arg ...)))])))

(define-syntax safe/unsafe
  (lambda (form)
    (syntax-case form ()
      ((_ unopt opt)
       (= (optimize-level) 3)
       (syntax opt))
      ((_ unopt opt)
       (syntax unopt)))))

(define-integrable instance?
  (lambda (x)
    (and (vector? x)
         (fx>= (vector-length x) 2)
         (class? (vector-ref x 1)))))

(module ((send instance-dispatch-table) (send-apply instance-dispatch-table))

  (module ((instance-dispatch-table safe-instance-dispatch-table))
    (define safe-instance-dispatch-table
      (lambda (x who)
        (unless (instance? x) (assertion-violationf who "~s is not an instance" x))
        (vector-ref x 0)))
    (define-integrable
      instance-dispatch-table
      (lambda (x who)
        (safe/unsafe
          (safe-instance-dispatch-table x who)
          (vector-ref x 0)))))
  
  (define-syntax send
    (lambda (form)
      (syntax-case form ()
        [(_ obj msg arg ...)
         (identifier? (syntax msg))
         (syntax
           (let ([t obj])
             ((instance-dispatch-table t 'msg) 'msg t arg ...)))])))
  
  (define-syntax send-apply
    (lambda (form)
      (syntax-case form ()
        [(_ obj msg arg ...)
         (identifier? (syntax msg))
         (syntax
           (let ([t obj])
             (apply (instance-dispatch-table t 'msg) 'msg t arg ...)))])))
)

;; Make sure we get all the base ivars included in our list of ivars for
;; building the instance.
;;
;; have a local version of the send macro that checks to see if it's
;; calling a private guy (in which case we don't need to pass msg)
;; same probably true of protected guys
;; decide whether it makes sense to close over the dispatch table
;; (maybe already am) so that the internal send can call public
;; generic guys via the usual send, but without the vector-ref
;; for private and protected guys we have more regs free for args.

(define-syntax define-class
 (let ()
   (define gen-name
     (lambda (id)
       (datum->syntax-object id
         (gensym (symbol->string (syntax-object->datum id))))))
   (lambda (frm)
    (syntax-case frm (ivars inherited inheritable protected private public)
      [(dc (name . mfmls) (basename . mactls)
               (ivars (i v) ...)
               (inherited inhtd ...)
               (inheritable inhtbl ...)
               (private
                 [priv-name priv-fmls . priv-body] ...)
               (protected
                 [prot-name prot-fmls . prot-body] ...)
               (public
                 [pub-name pub-fmls . pub-body] ...))
       (with-syntax ((^... (syntax (... ...)))
                     (residual (gen-name (syntax name))))
       ; egregious hack to provide better warning message when base class hasn't been defined
       ; We get bad error message otherwise because all this expands into what appears to be
       ; a call (since base isn't defined, it's not recognized as macro) with a bunch of funky
       ; arguments.  Since it looks like a call, those args may get evaluated first and we'll
       ; get confusing error messages.  (see e-mail from JohnZ)
       ;
       ; Need better way to fix this in macro system -- in particular, this punts when we're
       ; in an internal definition.
       (let ((symbol (syntax-object->datum #'basename)))
;(printf "RE-ENABLE THIS CHECK\n")
         (when #f ; (free-identifier=? #'basename (datum->syntax-object #'toplevelsymbol symbol))
           ; don't try this if we're internally defined
           (when (eq? symbol (expand #'basename))
             (assertion-violationf #f "base class ~s undefined" symbol))))
       (syntax
         (module ((name residual))
           (define residual
             (basename make-subclass (name . mfmls) mactls
               (ivars (i v) ...)
               (inherited inhtd ...)
               (inheritable inhtbl ...)
               (private
                 [priv-name priv-fmls . priv-body] ...)
               (protected
                 [prot-name prot-fmls . prot-body] ...)
               (public
                 [pub-name pub-fmls . pub-body] ...)))

           (define-syntax name
             (lambda (form)
               (syntax-case form (quote real-make-subclass make-subclass make-instance
                                  ivars inherited inheritable
                                  protected private public)
                 (x (identifier? form) (syntax residual))
                 ((_ real-make-subclass (ivar ^...) (prot ^...) . rest)
;; presumably the fact that we list the inherited ivars prevents us from
;; having to do the swl:id-union here.
;; Note that instead of actually doing an swl:id-union we figure out which
;; are new protected guys and tack them on to the end of the prots list.
;; Should eventually revise this macro so that this computation is done
;; just once.
                  (with-syntax ((new-prot (swl:id-setrember
                                             (syntax (prot-name ...))
                                             (syntax (prot ^...)))))
                    (syntax (basename real-make-subclass
                              (i ... ivar ^...)
                              (prot-name ... . new-prot) . rest))))
                 ((_ 'make-instance residual (e ^...))
                  (syntax (lambda mfmls (swl:build-application
                    (basename 'make-instance residual (v ... e ^...)) . mactls))))
                 ((_ make-subclass (scname . _mfmls) _mactls
                     (ivars (_i _v) ^...)
                     (inherited _inhtd ^...)
                     (inheritable _inhtbl ^...)
                     (private
                       [_priv-name _priv-fmls . _priv-body] ^...)
                     (protected
                       [_prot-name _prot-fmls . _prot-body] ^...)
                     (public
                       [_pub-name _pub-fmls . _pub-body] ^...))
                  (let ((base-inhtbl (syntax (inhtbl ...)))
                        (sub-ivars (syntax (_i ^...)))
                        (sub-inhtd (syntax (_inhtd ^...)))
                        (sub-inhtbl (syntax (_inhtbl ^...))))
                    (issue-syntax-error-msgs
                      ((not (swl:id-subset? sub-inhtd base-inhtbl))
                       (warningf 'define-class
                         "~s attempts to inherit uninheritable ivars ~s from ~s"
                         (syntax-object->datum (syntax scname))
                         (syntax-object->datum (swl:id-setrember base-inhtbl sub-inhtd))
                         (syntax-object->datum (syntax name))))
                      ((not (null? (swl:id-intersect sub-inhtd sub-ivars)))
                       (warningf 'define-class
                         "inherited ivars ~s overlap ivars"
                         (syntax-object->datum (swl:id-intersect sub-inhtd sub-ivars))))
                      ((not (swl:id-subset? sub-inhtbl (append sub-ivars sub-inhtd)))
                       (warningf 'define-class
                         "set of inheritable ivars ~s is not a subset of the ivars and inherited vars ~s"
                         (syntax-object->datum sub-inhtbl)
                         (syntax-object->datum (append sub-ivars sub-inhtd)))))
                      (syntax (basename real-make-subclass (i ...) (prot-name ...)
                              residual basename
                              (scname . _mfmls) (name . _mactls)
                              (ivars (_i _v) ^...)
                              (inherited _inhtd ^...)
                              (inheritable _inhtbl ^...)
                              (private
                                [_priv-name _priv-fmls . _priv-body] ^...)
                              (protected
                                [_prot-name _prot-fmls . _prot-body] ^...)
                              (public
                                [_pub-name _pub-fmls . _pub-body] ^...)))))))
                           ))))]))))

;; plays a crucial role in the expansion process

(define-syntax protected-start-index (identifier-syntax 3)) ;; see diagram above showing instance / class layout
(define-syntax ivar-start-index (identifier-syntax 2))

(define-syntax <bootstrap>
  (let ()
    (define enumerate
      (lambda (start ls)
        (if (null? ls)
            '()
            (cons start (enumerate (+ start 1) (cdr ls))))))
  (lambda (form)
    (syntax-case form (quote real-make-subclass make-instance make-subclass
                        ivars inherited inheritable protected private public)
      [(_ make-subclass . whatever)
       (syntax
         (let ([err
                (lambda (msg . args)
                  (assertion-violationf 'dispatch
                    "unknown arity ~s public method ~s"
                    (- (length args) 1)
                    msg))])
           (swl:make-class err)))]
      [(_ 'make-instance residual (e ...))
       (syntax (lambda () (vector (class-dispatch-table residual) residual e ...)))]
      [(_ real-make-subclass
          (ivar ...)
          (prot ...)
          base-residual
          basename
          (scname . _mfmls)
          (name . _mactls)
          (ivars (_i _v) ...)
          (inherited _inhtd ...)
          (inheritable _inhtbl ...)
          (private (_priv-name _priv-fmls . _priv-body) ...)
          (protected (_prot-name _prot-fmls . _prot-body) ...)
          (public (_pub-name _pub-fmls . _pub-body) ...))
       (let ([base-ivars (syntax (ivar ...))]
             [base-prots (syntax (prot ...))]
;; compute these sets via mrvs
             [new-prot
              (swl:id-setrember (syntax (prot ...)) (syntax (_prot-name ...)))]
             [redef-prot
              (swl:id-intersect (syntax (prot ...)) (syntax (_prot-name ...)))])
#;       (when  swl:define-class-verbose
           (printf "~n------ ~s -------~n" (syntax-object->datum (syntax scname)))
           (printf "base-ivars = ~s~nbase-prots = ~s~n"
             (syntax-object->datum (syntax (ivar ...)))
             (syntax-object->datum (syntax (prot ...))))
           (printf "new-prot = ~s~nredef-prot = ~s~n"
             (syntax-object->datum new-prot)
             (syntax-object->datum redef-prot))
           (printf "public = ~s\n" (syntax-object->datum #'(_pub-name ...))))
         (let ([ivar-len (length base-ivars)]
               [prot-len (length base-prots)]
               [ihtd-prot (swl:id-setrember redef-prot base-prots)])
           (with-syntax ([set! (datum->syntax-object (syntax scname) 'set!)]
                         [send (datum->syntax-object (syntax scname) 'send)]
                         [send-base
                          (datum->syntax-object (syntax scname) 'send-base)]
                         [self (datum->syntax-object (syntax scname) 'self)]
                         [(new-prot ...) new-prot]
                         [(redef-prot ...) redef-prot]
                         [(base-prot-index ...)
                          (enumerate
                            protected-start-index
                            (syntax (prot ...)))]
                         [(new-prot-index ...)
                          (enumerate
                            (+ protected-start-index prot-len)
                            new-prot)]
                         [(redef-prot-index ...)
                          (map (lambda (x)
                                 (+ protected-start-index
                                    (or (swl:id-list-index x base-prots)
                                        (assertion-violationf 'redef-prot-index
                                          "missing ~s"
                                          (syntax-object->datum x)))))
                               redef-prot)]
                         [(ihtd-prot ...) ihtd-prot]
                         [(ihtd-prot-index ...)
                          (map (lambda (x)
                                 (+ protected-start-index
                                    (swl:id-list-index x base-prots)))
                               ihtd-prot)]
                         [(privtmp ...)
                          (generate-temporaries (syntax (_priv-name ...)))]
                         [(ivar-index ...)
                          (enumerate
                            (+ ivar-start-index ivar-len)
                            (syntax (_i ...)))]
                         [(ihtd-index ...)
                          (map (lambda (x)
                                 (+ ivar-start-index
                                    (or (swl:id-list-index x base-ivars)
                                        (assertion-violationf 'inhtd-index
                                          "missing ~s"
                                          (syntax-object->datum x)))))
                               (syntax (_inhtd ...)))])
             (syntax
               (let-syntax ((self (identifier-syntax self)))
                 (let-syntax
                   ((_i (identifier-syntax
                          (id (vector-ref self ivar-index))
                          ((set! id val) (vector-set! self ivar-index val))))
                    ...
                    (_inhtd
                      (identifier-syntax
                        (id (vector-ref self ihtd-index))
                        ((set! id val) (vector-set! self ihtd-index val))))
                    ...
                    (method
                      (syntax-rules ()
                        [(kwd fmls . body)
                         (lambda (inst . fmls)
                           (fluid-let-syntax
                             ((self (identifier-syntax inst)))
                             .
                             body))])))
                   (let-syntax
                     ((send-base
                        (lambda (form)
                          (syntax-case form (prot ...)
                            [(kwd prot . args)
                             ;; prot are the protecteds of the base class, so
                             ;; send-base is kosher.  for protecteds, self is
                             ;; an implicit argument
                             (syntax
                               ((vector-ref name base-prot-index) self . args))]
                            ...
                            [(kwd obj msg . args)
                             (identifier? (syntax msg))
                             (syntax
                               ((class-dispatch-table name) 'msg obj . args))])))
                      (send (let ([blacklisted?
                                   (let ([list
                                          (syntax
                                            (_priv-name ...
                                              _prot-name ...
                                              ihtd-prot ...))])
                                     (lambda (who)
                                       (swl:id-list-index who list)))])
                              (lambda (x)
                                (syntax-case x ()
                                  [(kwd obj msg . args)
                                   (identifier? (syntax msg))
                                   (if (and (identifier? #'obj)
                                            (free-identifier=? #'obj #'self)
                                            (blacklisted? (syntax msg)))
                                       (syntax-error x
                                         "send syntax not permitted for protected/private methods")
                                       (syntax (send obj msg . args)))])))))
                     (let-syntax
                       ((wire-prot
                          (syntax-rules (_prot-name ... ihtd-prot ...)
                            [(kwd _prot-name)
                             (method _prot-fmls . _prot-body)]
                            ...
                            [(kwd ihtd-prot)
                             (vector-ref name ihtd-prot-index)]
                            ...)))
                       (fluid-let-syntax
                         ((ihtd-prot
                            (lambda (x)
                              (syntax-case x ()
                                [(_ . args)
                                 #'((vector-ref (vector-ref self 1) ihtd-prot-index) self . args)]
                                [id
                                 (identifier? #'id)
                                 #'(lambda x
                                     (apply
                                       (vector-ref (vector-ref self 1) ihtd-prot-index)
                                       self x))])))
                          ...
                          (new-prot
                            (lambda (x)
                              (syntax-case x ()
                                [(_ . args)
                                 #'((vector-ref (vector-ref self 1) new-prot-index) self . args)]
                                [id (identifier? #'id)
                                 #'(lambda x
                                     (apply (vector-ref (vector-ref self 1) new-prot-index) self x))])))
                          ...
                          (redef-prot
                            (lambda (x)
                              (syntax-case x ()
                                [(_ . args)
                                 #'((vector-ref (vector-ref self 1) redef-prot-index) self . args)]
                                [id (identifier? #'id)
                                 #'(lambda x
                                     (apply (vector-ref (vector-ref self 1) redef-prot-index) self x))])))
                          ...)
                         (letrec ([privtmp
                                   (fluid-let-syntax
                                     ((_priv-name
                                       (lambda (x)
                                         (syntax-case x ()
                                           [(_ . args) #'(privtmp self . args)]
                                           [id
                                            (identifier? #'id)
                                            #'(lambda _priv-fmls (id . _priv-fmls))])))
                                      ...)
                                     (method _priv-fmls . _priv-body))]
                                  ...)
                           (fluid-let-syntax
                             ((_priv-name
                                (lambda (x)
                                  (syntax-case x ()
                                    [(_ . args) #'(privtmp self . args)]
                                    [id
                                     (identifier? #'id)
                                     #'(lambda _priv-fmls (id . _priv-fmls))])))
                              ...)
                             (rec the-class
                               (let ([pdt (class-dispatch-table name)])
                                 (swl:make-class
                                   (swl:dispatch-table self
                                     pdt
                                     (_pub-name _pub-fmls . _pub-body)
                                     ...
                                     (isa? (x)
                                           (or (eq? x the-class)
                                               (send-base self isa? x))))
                                   pdt
                                   (wire-prot prot)
                                   ...
                                   (wire-prot new-prot)
                                   ...))
                                  ))))))))))))]
      [id (identifier? #'id) #'(void)]))))  ; so that we don't get error about <bootstrap> undefined when defining <root>

(define-class (<root>) (<bootstrap>)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public))

(define-class (<base>) (<root>)
  (ivars)
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    [isa? (x) #f]
    [print () (display "#<instance>")]
    [print (op) (display "#<instance>" op)]))

)

(eval-when (compile)
(printf "~nExpect warning that isa? is unreachable:~n")
)

(eval-when (compile eval) (internal-defines-as-letrec* #f))

#!eof


; ah, suspect the problem is transformers-only env

 (let ([zzx 4])
   (import swl:oop)
   (define-class (foo) (<base>)
     (ivars)
     (inherited)
     (inheritable)
     (private (getx () zzx))
     (protected)
     (public))
   (list (make foo)))

; might be since following works:

 (let-syntax ([zzx (identifier-syntax 4)])
   (import swl:oop)
   (define-class (foo) (<base>)
     (ivars)
     (inherited)
     (inheritable)
     (private (getx () zzx))
     (protected)
     (public))
   (list (make foo)))

 (let-syntax ([zzx (identifier-syntax 4)])
   (import swl:oop)
   (define-class (foo) (<base>)
     (ivars)
     (inherited)
     (inheritable)
     (private)
     (protected)
     (public (getx () zzx)))
   (list (make foo)))

; after patching syntax.ss to make transformers-only-env into the
; identity function, the following produced an invalid memory reference
; in cp0 --- maybe x somehow appeared to be a lexical but wasn't bound
;  probably because we'd hacked syntax.ss

; what if it's some strangeness of these deferred guys.
; let's try disabling deferred guys.
; no luck.

; Wait.  It might be a problem of using ctdef.

 (let ([x 4])
   (import swl:oop)
   (define-class (foo) (<base>)
     (ivars)
     (inherited)
     (inheritable)
     (private)
     (protected)
     (public (go () (pretty-print x))))
   (list (make foo) (lambda (v) (set! x v))))

