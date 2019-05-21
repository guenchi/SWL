;; Copyright (c) 1996 Oscar Waddell
;;
;; See the file "Notice" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Event method bindings
;;

; Note:  as a hack until I decide what to do about protecting these
;        callback procs from being collected, I'm making top-level bindings
;        for them

; Tk-event            SWL method and args
;-----------------------------------------------------------------------------
; Configure      -->  configure (width height)
; KeyPress       -->  key-press (key modifiers)
; KeyRelease     -->  key-release (key modifiers)
; Enter          -->  mouse-enter (x y modifiers)
; Leave          -->  mouse-leave (x y modifiers)
; ButtonPress    -->  mouse-press (x y modifiers)
; ButtonRelease  -->  mouse-release (x y modifiers)
; Motion         -->  mouse-motion (x y modifiers)

;; Much of this will be hidden once we have the module system.

;; We convert key events to Scheme characters if possible, else we leave
;; in a form that must be processed by event-case.

(define decimal->key
  (lambda (n)
    (if (fx< 0 n 256)
        (integer->char n)
        (case n
         ; rkd: removed #xffff (delete) from backspace line
          [(#xff08) #\backspace]
          [(#xff09) #\tab]
          [(#xff0a) #\newline]
          [(#xff0d) #\return]
          [else n]))))

;; These are associated with |SWL| class in init.ss
;; should be held w/ strong pointers.

(define configure-cbproc
  (swl:callback-lambda (widget width height)
    (when widget (send widget configure width height))))

(define enter-cbproc
  (swl:callback-lambda (widget x y state)
    (when widget (send widget mouse-enter x y state))))

(define leave-cbproc
  (swl:callback-lambda (widget x y state)
    (when widget (send widget mouse-leave x y state))))

(define keypress-cbproc
  (swl:callback-lambda (widget keysym-decimal state)
    (when widget
      (send widget key-press (decimal->key keysym-decimal) state))))

(define keyrelease-cbproc
  (swl:callback-lambda (widget keysym-decimal state)
    (when widget
      (send widget key-release (decimal->key keysym-decimal) state))))

;; or-ing button number into the modifiers being passed to mouse-press method
(define mousepress-cbproc
  (swl:callback-lambda (widget x y button-number state)
    (when widget
      (send widget mouse-press x y
        (fxlogor (fxsll 1 (fx+ 7 button-number)) state)))))

;; here button-number should already be implied in the state field, though
;; we may be missing exactly the button that was released which would be
;; bogus
(define mouserelease-cbproc
  (swl:callback-lambda (widget x y state)
    (when widget
      (send widget mouse-release x y state))))

;; the proplist hackery here is until I rethink the stuff that collects
;; garbage cbprocs.  obviously don't want these guys to be garbage,
;; so their classes should be hanging onto them w/ strong pointers...

(define multi-mousepress-cbproc
  (lambda (bit)
(let ((cb
    (swl:callback-lambda (widget x y button-number state)
      (when widget
        (send widget mouse-press x y
          (fxlogor bit (fxlogor (fxsll 1 (fx+ 7 button-number)) state)))))
))
(putprop 'SWL 'bogosity (cons cb (getprop 'SWL 'bogosity)))
cb)))

(define multi-mouserelease-cbproc
  (lambda (bit)
(let ((cb
    (swl:callback-lambda (widget x y state)
      (when widget
        (send widget mouse-release x y (fxlogor bit state))))
))
(putprop 'SWL 'bogosity (cons cb (getprop 'SWL 'bogosity)))
cb)))

(define mousemotion-cbproc
  (swl:callback-lambda (widget x y state)
    (when widget (send widget mouse-motion x y state))))

(define swl:parse-filenames
  (when (memq (machine-type) '(i3osx ppcosx ti3osx tppcosx))
    (lambda (s)
      (let ([n (string-length s)])
        (define (sx i ls) (cons (list->string (reverse ls)) (s0 i)))
        (define (s0 i)
          (if (fx= i n)
              '()
              (let ([c (string-ref s i)])
                (case c
                  [(#\space) (s0 (fx+ i 1))]
                  [(#\{) (sbrace (fx+ i 1) '())]
                  [(#\\) (sslash (fx+ i 1) '())]
                  [else (s1 (fx+ i 1) (list c))]))))
        (define (s1 i ls)
          (if (fx= i n)
              (sx i ls)
              (let ([c (string-ref s i)])
                (case c
                  [(#\\) (sslash (fx+ i 1) ls)]
                  [(#\space) (sx (fx+ i 1) ls)]
                  [else (s1 (fx+ i 1) (cons c ls))]))))
        (define (sslash i ls)
          (if (fx= i n) ; shouldn't happen
              (sx i ls)
              (s1 (fx+ i 1) (cons (string-ref s i) ls))))
        (define (sbrace i ls)
          (if (fx= i n) ; shouldn't happen
              (sx i ls)
              (let ([c (string-ref s i)])
                (case c
                  [(#\}) (sx (fx+ i 1) ls)]
                  [else (sbrace (fx+ i 1) (cons c ls))]))))
        (s0 0)))))

(define new-edit-cbproc
  (when (memq (machine-type) '(i3osx ppcosx ti3osx tppcosx))
   ; hack to start up edit windows on MacOS X, using the files_to_edit
   ; tcl variable set by aqua.tcl's ::tk::mac::OpenDocument whenever
   ; file is double-clicked or dragged onto swl icon
    (swl:callback-lambda ()
      (define (start-editor ls)
        (swl:begin-application
          (lambda (token)
            (define thread
              (thread-fork
                (lambda ()
                  (call/cc
                    (lambda (k)
                      (parameterize ([current-directory (current-directory)]
                                     [abort-handler k]
                                     [exit-handler k]
                                     [interrupt-handler k]
                                     [reset-handler k]
                                     [exception-handler-continuation k])
                        (map new-edit ls))))
                  (set! thread #f)
                  (swl:end-application token))))
            (lambda ()
              (when thread
                (case (warning-dialog
                        #f
                        (format "Still processing new-edit commands")
                        '(|keep processing| |stop processing|))
                  [(|stop processing|)
                   (on-error
                     'ignore
                     (thread-kill thread)
                     (set! thread #f))
                   (swl:end-application token)]))))))
      (let ([ls (swl:parse-filenames
                  (critical-section
                    (let ([s (swl:tcl-eval 'set 'files_to_edit)])
                      (swl:tcl-eval 'set 'files_to_edit "")
                      s)))])
        (unless (null? ls)
          (start-editor ls))))))

;; Given a decimal keysym and state, this returns
;;   char    for ascii values
;;   symbol  for special things like control_l, alt_r, shift_l
;; make this more efficient, or make sure the compiler is smart enough to

;; keep this up to date w/ key-press documentation in base2.ss

;; The following was semi-automatically generated by editing the
;; output of
;;   sed -f sed-script < /usr/include/X11/keysymdef.h
;; where sed-script contains:
;;   s/#define XK_\(.*\).*0x\([a-fA-F0-9]*\).*/(\1 . #x\2)/


;(eval-when (compile load eval)

(ctdef key-translation
       '((backspace . #\backspace)
         (tab . #\tab)
         (linefeed . #\newline)
         (clear . 65291)
         (return . #\return)
         (pause . 65299)
         (scroll_lock . 65300)
         (sys_req . 65301)
         (escape . 65307)
         (delete . 65535)
         (multi_key . 65312)
         (kanji . 65313)
         (muhenkan . 65314)
         (henkan_mode . 65315)
         (henkan . 65315)
         (romaji . 65316)
         (hiragana . 65317)
         (katakana . 65318)
         (hiragana_katakana . 65319)
         (zenkaku . 65320)
         (hankaku . 65321)
         (zenkaku_hankaku . 65322)
         (touroku . 65323)
         (massyo . 65324)
         (kana_lock . 65325)
         (kana_shift . 65326)
         (eisu_shift . 65327)
         (eisu_toggle . 65328)
         (home . 65360)
         (left . 65361)
         (up . 65362)
         (right . 65363)
         (down . 65364)
         (prior . 65365)
         (page_up . 65365)
         (next . 65366)
         (page_down . 65366)
         (end . 65367)
         (begin . 65368)
         (select . 65376)
         (print . 65377)
         (execute . 65378)
         (insert . 65379)
         (undo . 65381)
         (redo . 65382)
         (menu . 65383)
         (find . 65384)
         (cancel . 65385)
         (help . 65386)
         (break . 65387)
         (mode_switch . 65406)
         (script_switch . 65406)
         (num_lock . 65407)
         (kp_space . 65408)
         (kp_tab . 65417)
         (kp_enter . 65421)
         (kp_f1 . 65425)
         (kp_f2 . 65426)
         (kp_f3 . 65427)
         (kp_f4 . 65428)
         (kp_home . 65429)
         (kp_left . 65430)
         (kp_up . 65431)
         (kp_right . 65432)
         (kp_down . 65433)
         (kp_prior . 65434)
         (kp_page_up . 65434)
         (kp_next . 65435)
         (kp_page_down . 65435)
         (kp_end . 65436)
         (kp_begin . 65437)
         (kp_insert . 65438)
         (kp_delete . 65439)
         (kp_equal . 65469)
         (kp_multiply . 65450)
         (kp_add . 65451)
         (kp_separator . 65452)
         (kp_subtract . 65453)
         (kp_decimal . 65454)
         (kp_divide . 65455)
         (kp_0 . 65456)
         (kp_1 . 65457)
         (kp_2 . 65458)
         (kp_3 . 65459)
         (kp_4 . 65460)
         (kp_5 . 65461)
         (kp_6 . 65462)
         (kp_7 . 65463)
         (kp_8 . 65464)
         (kp_9 . 65465)
         (f1 . 65470)
         (f2 . 65471)
         (f3 . 65472)
         (f4 . 65473)
         (f5 . 65474)
         (f6 . 65475)
         (f7 . 65476)
         (f8 . 65477)
         (f9 . 65478)
         (f10 . 65479)
         (f11 . 65480)
         (l1 . 65480)
         (f12 . 65481)
         (l2 . 65481)
         (f13 . 65482)
         (l3 . 65482)
         (f14 . 65483)
         (l4 . 65483)
         (f15 . 65484)
         (l5 . 65484)
         (f16 . 65485)
         (l6 . 65485)
         (f17 . 65486)
         (l7 . 65486)
         (f18 . 65487)
         (l8 . 65487)
         (f19 . 65488)
         (l9 . 65488)
         (f20 . 65489)
         (l10 . 65489)
         (f21 . 65490)
         (r1 . 65490)
         (f22 . 65491)
         (r2 . 65491)
         (f23 . 65492)
         (r3 . 65492)
         (f24 . 65493)
         (r4 . 65493)
         (f25 . 65494)
         (r5 . 65494)
         (f26 . 65495)
         (r6 . 65495)
         (f27 . 65496)
         (r7 . 65496)
         (f28 . 65497)
         (r8 . 65497)
         (f29 . 65498)
         (r9 . 65498)
         (f30 . 65499)
         (r10 . 65499)
         (f31 . 65500)
         (r11 . 65500)
         (f32 . 65501)
         (r12 . 65501)
         (f33 . 65502)
         (r13 . 65502)
         (f34 . 65503)
         (r14 . 65503)
         (f35 . 65504)
         (r15 . 65504)
         (shift_l . 65505)
         (shift_r . 65506)
         (control_l . 65507)
         (control_r . 65508)
         (caps_lock_l . 65509)
         (shift_lock . 65510)
         (meta_l . 65511)
         (meta_r . 65512)
         (alt_l . 65513)
         (alt_r . 65514)
         (super_l . 65515)
         (super_r . 65516)
         (hyper_l . 65517)
         (hyper_r . 65518)
         (aogonek . 417)
         (breve . 418)
         (lstroke . 419)
         (lcaron . 421)
         (sacute . 422)
         (scaron . 425)
         (scedilla . 426)
         (tcaron . 427)
         (zacute . 428)
         (zcaron . 430)
         (zabovedot . 431)
         (aogonek . 433)
         (ogonek . 434)
         (lstroke . 435)
         (lcaron . 437)
         (sacute . 438)
         (caron . 439)
         (scaron . 441)
         (scedilla . 442)
         (tcaron . 443)
         (zacute . 444)
         (doubleacute . 445)
         (zcaron . 446)
         (zabovedot . 447)
         (racute . 448)
         (abreve . 451)
         (lacute . 453)
         (cacute . 454)
         (ccaron . 456)
         (eogonek . 458)
         (ecaron . 460)
         (dcaron . 463)
         (dstroke . 464)
         (nacute . 465)
         (ncaron . 466)
         (odoubleacute . 469)
         (rcaron . 472)
         (uring . 473)
         (udoubleacute . 475)
         (tcedilla . 478)
         (racute . 480)
         (abreve . 483)
         (lacute . 485)
         (cacute . 486)
         (ccaron . 488)
         (eogonek . 490)
         (ecaron . 492)
         (dcaron . 495)
         (dstroke . 496)
         (nacute . 497)
         (ncaron . 498)
         (odoubleacute . 501)
         (udoubleacute . 507)
         (rcaron . 504)
         (uring . 505)
         (tcedilla . 510)
         (abovedot . 511)
         (hstroke . 673)
         (hcircumflex . 678)
         (iabovedot . 681)
         (gbreve . 683)
         (jcircumflex . 684)
         (hstroke . 689)
         (hcircumflex . 694)
         (idotless . 697)
         (gbreve . 699)
         (jcircumflex . 700)
         (cabovedot . 709)
         (ccircumflex . 710)
         (gabovedot . 725)
         (gcircumflex . 728)
         (ubreve . 733)
         (scircumflex . 734)
         (cabovedot . 741)
         (ccircumflex . 742)
         (gabovedot . 757)
         (gcircumflex . 760)
         (ubreve . 765)
         (scircumflex . 766)
         (kra . 930)
         (kappa . 930)
         (rcedilla . 931)
         (itilde . 933)
         (lcedilla . 934)
         (emacron . 938)
         (gcedilla . 939)
         (tslash . 940)
         (rcedilla . 947)
         (itilde . 949)
         (lcedilla . 950)
         (emacron . 954)
         (gcedilla . 955)
         (tslash . 956)
         (eng . 957)
         (eng . 959)
         (amacron . 960)
         (iogonek . 967)
         (eabovedot . 972)
         (imacron . 975)
         (ncedilla . 977)
         (omacron . 978)
         (kcedilla . 979)
         (uogonek . 985)
         (utilde . 989)
         (umacron . 990)
         (amacron . 992)
         (iogonek . 999)
         (eabovedot . 1004)
         (imacron . 1007)
         (ncedilla . 1009)
         (omacron . 1010)
         (kcedilla . 1011)
         (uogonek . 1017)
         (utilde . 1021)
         (umacron . 1022)
         (overline . 1150)
         (kana_fullstop . 1185)
         (kana_openingbracket . 1186)
         (kana_closingbracket . 1187)
         (kana_comma . 1188)
         (kana_conjunctive . 1189)
         (kana_middledot . 1189)
         (kana_wo . 1190)
         (kana_a . 1191)
         (kana_i . 1192)
         (kana_u . 1193)
         (kana_e . 1194)
         (kana_o . 1195)
         (kana_ya . 1196)
         (kana_yu . 1197)
         (kana_yo . 1198)
         (kana_tsu . 1199)
         (kana_tu . 1199)
         (prolongedsound . 1200)
         (kana_a . 1201)
         (kana_i . 1202)
         (kana_u . 1203)
         (kana_e . 1204)
         (kana_o . 1205)
         (kana_ka . 1206)
         (kana_ki . 1207)
         (kana_ku . 1208)
         (kana_ke . 1209)
         (kana_ko . 1210)
         (kana_sa . 1211)
         (kana_shi . 1212)
         (kana_su . 1213)
         (kana_se . 1214)
         (kana_so . 1215)
         (kana_ta . 1216)
         (kana_chi . 1217)
         (kana_ti . 1217)
         (kana_tsu . 1218)
         (kana_tu . 1218)
         (kana_te . 1219)
         (kana_to . 1220)
         (kana_na . 1221)
         (kana_ni . 1222)
         (kana_nu . 1223)
         (kana_ne . 1224)
         (kana_no . 1225)
         (kana_ha . 1226)
         (kana_hi . 1227)
         (kana_fu . 1228)
         (kana_hu . 1228)
         (kana_he . 1229)
         (kana_ho . 1230)
         (kana_ma . 1231)
         (kana_mi . 1232)
         (kana_mu . 1233)
         (kana_me . 1234)
         (kana_mo . 1235)
         (kana_ya . 1236)
         (kana_yu . 1237)
         (kana_yo . 1238)
         (kana_ra . 1239)
         (kana_ri . 1240)
         (kana_ru . 1241)
         (kana_re . 1242)
         (kana_ro . 1243)
         (kana_wa . 1244)
         (kana_n . 1245)
         (voicedsound . 1246)
         (semivoicedsound . 1247)
         (kana_switch . 65406)
         (arabic_comma . 1452)
         (arabic_semicolon . 1467)
         (arabic_question_mark . 1471)
         (arabic_hamza . 1473)
         (arabic_maddaonalef . 1474)
         (arabic_hamzaonalef . 1475)
         (arabic_hamzaonwaw . 1476)
         (arabic_hamzaunderalef . 1477)
         (arabic_hamzaonyeh . 1478)
         (arabic_alef . 1479)
         (arabic_beh . 1480)
         (arabic_tehmarbuta . 1481)
         (arabic_teh . 1482)
         (arabic_theh . 1483)
         (arabic_jeem . 1484)
         (arabic_hah . 1485)
         (arabic_khah . 1486)
         (arabic_dal . 1487)
         (arabic_thal . 1488)
         (arabic_ra . 1489)
         (arabic_zain . 1490)
         (arabic_seen . 1491)
         (arabic_sheen . 1492)
         (arabic_sad . 1493)
         (arabic_dad . 1494)
         (arabic_tah . 1495)
         (arabic_zah . 1496)
         (arabic_ain . 1497)
         (arabic_ghain . 1498)
         (arabic_tatweel . 1504)
         (arabic_feh . 1505)
         (arabic_qaf . 1506)
         (arabic_kaf . 1507)
         (arabic_lam . 1508)
         (arabic_meem . 1509)
         (arabic_noon . 1510)
         (arabic_ha . 1511)
         (arabic_heh . 1511)
         (arabic_waw . 1512)
         (arabic_alefmaksura . 1513)
         (arabic_yeh . 1514)
         (arabic_fathatan . 1515)
         (arabic_dammatan . 1516)
         (arabic_kasratan . 1517)
         (arabic_fatha . 1518)
         (arabic_damma . 1519)
         (arabic_kasra . 1520)
         (arabic_shadda . 1521)
         (arabic_sukun . 1522)
         (arabic_switch . 65406)
         (serbian_dje . 1697)
         (macedonia_gje . 1698)
         (cyrillic_io . 1699)
         (ukrainian_ie . 1700)
         (ukranian_je . 1700)
         (macedonia_dse . 1701)
         (ukrainian_i . 1702)
         (ukranian_i . 1702)
         (ukrainian_yi . 1703)
         (ukranian_yi . 1703)
         (cyrillic_je . 1704)
         (serbian_je . 1704)
         (cyrillic_lje . 1705)
         (serbian_lje . 1705)
         (cyrillic_nje . 1706)
         (serbian_nje . 1706)
         (serbian_tshe . 1707)
         (macedonia_kje . 1708)
         (byelorussian_shortu . 1710)
         (cyrillic_dzhe . 1711)
         (serbian_dze . 1711)
         (numerosign . 1712)
         (serbian_dje . 1713)
         (macedonia_gje . 1714)
         (cyrillic_io . 1715)
         (ukrainian_ie . 1716)
         (ukranian_je . 1716)
         (macedonia_dse . 1717)
         (ukrainian_i . 1718)
         (ukranian_i . 1718)
         (ukrainian_yi . 1719)
         (ukranian_yi . 1719)
         (cyrillic_je . 1720)
         (serbian_je . 1720)
         (cyrillic_lje . 1721)
         (serbian_lje . 1721)
         (cyrillic_nje . 1722)
         (serbian_nje . 1722)
         (serbian_tshe . 1723)
         (macedonia_kje . 1724)
         (byelorussian_shortu . 1726)
         (cyrillic_dzhe . 1727)
         (serbian_dze . 1727)
         (cyrillic_yu . 1728)
         (cyrillic_a . 1729)
         (cyrillic_be . 1730)
         (cyrillic_tse . 1731)
         (cyrillic_de . 1732)
         (cyrillic_ie . 1733)
         (cyrillic_ef . 1734)
         (cyrillic_ghe . 1735)
         (cyrillic_ha . 1736)
         (cyrillic_i . 1737)
         (cyrillic_shorti . 1738)
         (cyrillic_ka . 1739)
         (cyrillic_el . 1740)
         (cyrillic_em . 1741)
         (cyrillic_en . 1742)
         (cyrillic_o . 1743)
         (cyrillic_pe . 1744)
         (cyrillic_ya . 1745)
         (cyrillic_er . 1746)
         (cyrillic_es . 1747)
         (cyrillic_te . 1748)
         (cyrillic_u . 1749)
         (cyrillic_zhe . 1750)
         (cyrillic_ve . 1751)
         (cyrillic_softsign . 1752)
         (cyrillic_yeru . 1753)
         (cyrillic_ze . 1754)
         (cyrillic_sha . 1755)
         (cyrillic_e . 1756)
         (cyrillic_shcha . 1757)
         (cyrillic_che . 1758)
         (cyrillic_hardsign . 1759)
         (cyrillic_yu . 1760)
         (cyrillic_a . 1761)
         (cyrillic_be . 1762)
         (cyrillic_tse . 1763)
         (cyrillic_de . 1764)
         (cyrillic_ie . 1765)
         (cyrillic_ef . 1766)
         (cyrillic_ghe . 1767)
         (cyrillic_ha . 1768)
         (cyrillic_i . 1769)
         (cyrillic_shorti . 1770)
         (cyrillic_ka . 1771)
         (cyrillic_el . 1772)
         (cyrillic_em . 1773)
         (cyrillic_en . 1774)
         (cyrillic_o . 1775)
         (cyrillic_pe . 1776)
         (cyrillic_ya . 1777)
         (cyrillic_er . 1778)
         (cyrillic_es . 1779)
         (cyrillic_te . 1780)
         (cyrillic_u . 1781)
         (cyrillic_zhe . 1782)
         (cyrillic_ve . 1783)
         (cyrillic_softsign . 1784)
         (cyrillic_yeru . 1785)
         (cyrillic_ze . 1786)
         (cyrillic_sha . 1787)
         (cyrillic_e . 1788)
         (cyrillic_shcha . 1789)
         (cyrillic_che . 1790)
         (cyrillic_hardsign . 1791)
         (greek_alphaaccent . 1953)
         (greek_epsilonaccent . 1954)
         (greek_etaaccent . 1955)
         (greek_iotaaccent . 1956)
         (greek_iotadiaeresis . 1957)
         (greek_omicronaccent . 1959)
         (greek_upsilonaccent . 1960)
         (greek_upsilondieresis . 1961)
         (greek_omegaaccent . 1963)
         (greek_accentdieresis . 1966)
         (greek_horizbar . 1967)
         (greek_alphaaccent . 1969)
         (greek_epsilonaccent . 1970)
         (greek_etaaccent . 1971)
         (greek_iotaaccent . 1972)
         (greek_iotadieresis . 1973)
         (greek_iotaaccentdieresis . 1974)
         (greek_omicronaccent . 1975)
         (greek_upsilonaccent . 1976)
         (greek_upsilondieresis . 1977)
         (greek_upsilonaccentdieresis . 1978)
         (greek_omegaaccent . 1979)
         (greek_alpha . 1985)
         (greek_beta . 1986)
         (greek_gamma . 1987)
         (greek_delta . 1988)
         (greek_epsilon . 1989)
         (greek_zeta . 1990)
         (greek_eta . 1991)
         (greek_theta . 1992)
         (greek_iota . 1993)
         (greek_kappa . 1994)
         (greek_lamda . 1995)
         (greek_lambda . 1995)
         (greek_mu . 1996)
         (greek_nu . 1997)
         (greek_xi . 1998)
         (greek_omicron . 1999)
         (greek_pi . 2000)
         (greek_rho . 2001)
         (greek_sigma . 2002)
         (greek_tau . 2004)
         (greek_upsilon . 2005)
         (greek_phi . 2006)
         (greek_chi . 2007)
         (greek_psi . 2008)
         (greek_omega . 2009)
         (greek_alpha . 2017)
         (greek_beta . 2018)
         (greek_gamma . 2019)
         (greek_delta . 2020)
         (greek_epsilon . 2021)
         (greek_zeta . 2022)
         (greek_eta . 2023)
         (greek_theta . 2024)
         (greek_iota . 2025)
         (greek_kappa . 2026)
         (greek_lamda . 2027)
         (greek_lambda . 2027)
         (greek_mu . 2028)
         (greek_nu . 2029)
         (greek_xi . 2030)
         (greek_omicron . 2031)
         (greek_pi . 2032)
         (greek_rho . 2033)
         (greek_sigma . 2034)
         (greek_finalsmallsigma . 2035)
         (greek_tau . 2036)
         (greek_upsilon . 2037)
         (greek_phi . 2038)
         (greek_chi . 2039)
         (greek_psi . 2040)
         (greek_omega . 2041)
         (greek_switch . 65406)
         (leftradical . 2209)
         (topleftradical . 2210)
         (horizconnector . 2211)
         (topintegral . 2212)
         (botintegral . 2213)
         (vertconnector . 2214)
         (topleftsqbracket . 2215)
         (botleftsqbracket . 2216)
         (toprightsqbracket . 2217)
         (botrightsqbracket . 2218)
         (topleftparens . 2219)
         (botleftparens . 2220)
         (toprightparens . 2221)
         (botrightparens . 2222)
         (leftmiddlecurlybrace . 2223)
         (rightmiddlecurlybrace . 2224)
         (topleftsummation . 2225)
         (botleftsummation . 2226)
         (topvertsummationconnector . 2227)
         (botvertsummationconnector . 2228)
         (toprightsummation . 2229)
         (botrightsummation . 2230)
         (rightmiddlesummation . 2231)
         (lessthanequal . 2236)
         (notequal . 2237)
         (greaterthanequal . 2238)
         (integral . 2239)
         (therefore . 2240)
         (variation . 2241)
         (infinity . 2242)
         (nabla . 2245)
         (approximate . 2248)
         (similarequal . 2249)
         (ifonlyif . 2253)
         (implies . 2254)
         (identical . 2255)
         (radical . 2262)
         (includedin . 2266)
         (includes . 2267)
         (intersection . 2268)
         (union . 2269)
         (logicaland . 2270)
         (logicalor . 2271)
         (partialderivative . 2287)
         (function . 2294)
         (leftarrow . 2299)
         (uparrow . 2300)
         (rightarrow . 2301)
         (downarrow . 2302)
         (blank . 2527)
         (soliddiamond . 2528)
         (checkerboard . 2529)
         (ht . 2530)
         (ff . 2531)
         (cr . 2532)
         (lf . 2533)
         (nl . 2536)
         (vt . 2537)
         (lowrightcorner . 2538)
         (uprightcorner . 2539)
         (upleftcorner . 2540)
         (lowleftcorner . 2541)
         (crossinglines . 2542)
         (horizlinescan1 . 2543)
         (horizlinescan3 . 2544)
         (horizlinescan5 . 2545)
         (horizlinescan7 . 2546)
         (horizlinescan9 . 2547)
         (leftt . 2548)
         (rightt . 2549)
         (bott . 2550)
         (topt . 2551)
         (vertbar . 2552)
         (emspace . 2721)
         (enspace . 2722)
         (em3space . 2723)
         (em4space . 2724)
         (digitspace . 2725)
         (punctspace . 2726)
         (thinspace . 2727)
         (hairspace . 2728)
         (emdash . 2729)
         (endash . 2730)
         (signifblank . 2732)
         (ellipsis . 2734)
         (doubbaselinedot . 2735)
         (onethird . 2736)
         (twothirds . 2737)
         (onefifth . 2738)
         (twofifths . 2739)
         (threefifths . 2740)
         (fourfifths . 2741)
         (onesixth . 2742)
         (fivesixths . 2743)
         (careof . 2744)
         (figdash . 2747)
         (leftanglebracket . 2748)
         (decimalpoint . 2749)
         (rightanglebracket . 2750)
         (marker . 2751)
         (oneeighth . 2755)
         (threeeighths . 2756)
         (fiveeighths . 2757)
         (seveneighths . 2758)
         (trademark . 2761)
         (signaturemark . 2762)
         (trademarkincircle . 2763)
         (leftopentriangle . 2764)
         (rightopentriangle . 2765)
         (emopencircle . 2766)
         (emopenrectangle . 2767)
         (leftsinglequotemark . 2768)
         (rightsinglequotemark . 2769)
         (leftdoublequotemark . 2770)
         (rightdoublequotemark . 2771)
         (prescription . 2772)
         (minutes . 2774)
         (seconds . 2775)
         (latincross . 2777)
         (hexagram . 2778)
         (filledrectbullet . 2779)
         (filledlefttribullet . 2780)
         (filledrighttribullet . 2781)
         (emfilledcircle . 2782)
         (emfilledrect . 2783)
         (enopencircbullet . 2784)
         (enopensquarebullet . 2785)
         (openrectbullet . 2786)
         (opentribulletup . 2787)
         (opentribulletdown . 2788)
         (openstar . 2789)
         (enfilledcircbullet . 2790)
         (enfilledsqbullet . 2791)
         (filledtribulletup . 2792)
         (filledtribulletdown . 2793)
         (leftpointer . 2794)
         (rightpointer . 2795)
         (club . 2796)
         (diamond . 2797)
         (heart . 2798)
         (maltesecross . 2800)
         (dagger . 2801)
         (doubledagger . 2802)
         (checkmark . 2803)
         (ballotcross . 2804)
         (musicalsharp . 2805)
         (musicalflat . 2806)
         (malesymbol . 2807)
         (femalesymbol . 2808)
         (telephone . 2809)
         (telephonerecorder . 2810)
         (phonographcopyright . 2811)
         (caret . 2812)
         (singlelowquotemark . 2813)
         (doublelowquotemark . 2814)
         (cursor . 2815)
         (leftcaret . 2979)
         (rightcaret . 2982)
         (downcaret . 2984)
         (upcaret . 2985)
         (overbar . 3008)
         (downtack . 3010)
         (upshoe . 3011)
         (downstile . 3012)
         (underbar . 3014)
         (jot . 3018)
         (quad . 3020)
         (uptack . 3022)
         (circle . 3023)
         (upstile . 3027)
         (downshoe . 3030)
         (rightshoe . 3032)
         (leftshoe . 3034)
         (lefttack . 3036)
         (righttack . 3068)
         (hebrew_doublelowline . 3295)
         (hebrew_aleph . 3296)
         (hebrew_bet . 3297)
         (hebrew_beth . 3297)
         (hebrew_gimel . 3298)
         (hebrew_gimmel . 3298)
         (hebrew_dalet . 3299)
         (hebrew_daleth . 3299)
         (hebrew_he . 3300)
         (hebrew_waw . 3301)
         (hebrew_zain . 3302)
         (hebrew_zayin . 3302)
         (hebrew_chet . 3303)
         (hebrew_het . 3303)
         (hebrew_tet . 3304)
         (hebrew_teth . 3304)
         (hebrew_yod . 3305)
         (hebrew_finalkaph . 3306)
         (hebrew_kaph . 3307)
         (hebrew_lamed . 3308)
         (hebrew_finalmem . 3309)
         (hebrew_mem . 3310)
         (hebrew_finalnun . 3311)
         (hebrew_nun . 3312)
         (hebrew_samech . 3313)
         (hebrew_samekh . 3313)
         (hebrew_ayin . 3314)
         (hebrew_finalpe . 3315)
         (hebrew_pe . 3316)
         (hebrew_finalzade . 3317)
         (hebrew_finalzadi . 3317)
         (hebrew_zade . 3318)
         (hebrew_zadi . 3318)
         (hebrew_qoph . 3319)
         (hebrew_kuf . 3319)
         (hebrew_resh . 3320)
         (hebrew_shin . 3321)
         (hebrew_taw . 3322)
         (hebrew_taf . 3322)
         (hebrew_switch . 65406)))
(ctdef keysym->keycode ;; This is used by event-case.
  (lambda (sym)
    (and (symbol? sym)
         (let ([x (assq sym key-translation)]) (and x (cdr x))))))
(ctdef keycode->keys
   ;; This will be used to write help program that lets programmer
   ;; hit a key on the keyboard and find out what event-case pattern
   ;; would match it.
  (lambda (n)
    (if (fx< 0 n 256)
        (integer->char n)
        (let find ([ls key-translation] [keys '()])
          (cond
            [(null? ls) keys]
            [(eqv? (cdar ls) n) (find (cdr ls) (cons (caar ls) keys))]
            [else (find (cdr ls) keys)])))))

;;   pattern  -->  pat | pat  or pattern
;;       pat  -->  keys | modifiers | modifiers keys
;;      keys  -->  ascii | keysym
;;     ascii  -->  char | char - char
;; modifiers  -->  mod | mod modifiers
;;       mod  -->  left-button | right-button | middle-button | control | alt |
;;                 shift | caps_lock | num_lock

;; eventually compile patterns for most efficient matching, and warn if
;; there are unreachable cases
;; 
;; current syntax:
;;
;; (event-case ((key= key) (modifier= modifier))
;;   (([control alt caps_lock #\a - #\z])
;;     ...)
;;   (([left-button alt Escape])
;;     ...)
;;   (([left] [#\h])
;;     ...)
;;   (([right] [control #\b] [#\h])
;;     ...)
;;   (([#\A - #\D])
;;     ...)
;;   (else
;;     ...))
;;
;; we could go for something more like:
;;   (({right} {control #\b} {#\h})
;;     ...)

;; once we have a module system this guy can be hidden
  (ctdef modifier->bitfield
    (case (machine-type)
      [(ppcnt i3nt)
       (lambda (m)
         (syntax-case m (shift caps_lock control alt num_lock left-button
                          middle-button right-button double triple)
           [shift              1]
           [caps_lock          2]
           [control            4]
           [num_lock           8]
           [alt          #x20000]  ; leaving in #x10 broke things
           [left-button    #x100]
           [middle-button  #x200]
           [right-button   #x400]
           [double         #x800]
           [triple        #x1000]
           [else #f]))]
      [(i3osx ppcosx)
       (lambda (m)
         (syntax-case m (shift caps_lock control alt num_lock left-button
                          middle-button right-button double triple
                          command)
           [shift              1]
           [caps_lock          2]
           [control            4]
           [alt               16] ; option  --- this is the %s field not %N
           [left-button    #x100]
           [middle-button  #x200]
           [right-button   #x400]
           [double         #x800]
           [triple        #x1000]
           [else #f]))]
      [else
       (lambda (m)
         (syntax-case m (shift caps_lock control alt num_lock left-button
                          middle-button right-button double triple)
           [shift 1]
           [caps_lock 2]
           [control 4]
           [alt 8]
           [num_lock 16]
           [left-button 256]
           [middle-button 512]
           [right-button 1024]
           [double 2048]
           [triple 4096]
           [else #f]))]))

(swl:api-syntax add-modifiers
  ;* \formdef{add-modifiers}{syntax}{(add-modifiers \var{keyword-or-modifier} \dots)}
  ;* \ret{a modifier set}
  ;* This macro constructs a set of modifiers from keywords such as \scheme{control}
  ;* and \scheme{left-button}, and from other modifiers.  The resulting set of
  ;* modifiers can be examined by \scheme{event-case}.  (See \ref{event-case})
  ;;
  ;; Note:  we need the 'bits in the syntax below to prevent source annotations
  ;;        from sticking to bits.  Probably should use datum->syntax-object
  ;;        instead anyway.  What's really interesting is that the source
  ;;        annotations (without the ') were showing me where this value
  ;;        came from.  Eg.  I'd get an error in fx= where the source pointed
  ;;        to a call (add-modifiers) somewhere in event-case.  It was almost
  ;;        helpful.
  (lambda (x)
    (syntax-case x ()
      ((_ x ...)
       (let loop ([xs (syntax (x ...))] [bits 0] [ids '()])
         (if (null? xs)
             (let loop ([ids ids]
                        [x (with-syntax ([bits bits]) (syntax 'bits))])
               (if (null? ids)
                   x
                   (loop (cdr ids)
                         (with-syntax ([x x] [id (car ids)])
                           (syntax (fxlogor x id))))))
             (let ([bit (modifier->bitfield (car xs))])
               (if bit
                   (loop (cdr xs) (fxlogor bit bits) ids)
                   (loop (cdr xs) bits (cons (car xs) ids))))))))))

(swl:api-syntax cancel-modifiers
  ;* \formdef{cancel-modifiers}{syntax}{(cancel-modifiers \var{keyword-or-modifier} \dots)}
  ;* \ret{a modifier set}
  ;* This macro returns the set of modifiers that results when subsequent
  ;* modifiers are removed from the first.
  ;* For example, if \scheme{x} is bound to a
  ;* set of modifiers, then \scheme{(cancel-modifiers x shift control)}
  ;* cancels the \scheme{shift} and \scheme{control} modifiers from \scheme{x}.
  ;* (See \ref{event-case})
  (lambda (x)
    (syntax-case x ()
      ((_) (syntax 0))
      ((_ x . y)
       (syntax (fxlogand (add-modifiers x) (fxlognot (add-modifiers . y))))))))

(swl:api-syntax event-case
  ;* \formdef{event-case}{syntax}{(event-case ((key= \var{x}) (modifier= \var{y})) ((pattern \dots) \var{e0} \var{e1} \dots) \dots)}\label{event-case}
  ;* \ret{see below}
  ;*
  ;* This macro simplifies pattern matching of events.
  ;* The \scheme{key=} and \scheme{modifier=} bindings indicate expressions that
  ;* evaluate to the key and modifier information from the event.  One
  ;* or the other may be omitted.
  ;* 
  ;* \scheme{pattern} is a non-empty list of zero or more modifiers
  ;* followed by an optional key specifier.  Modifiers are keywords
  ;* from the set \scheme{shift}, \scheme{control}, \scheme{alt}, \scheme{caps_lock},
  ;* \scheme{num_lock}, \scheme{double}, \scheme{triple},  \scheme{left-button},
  ;* \scheme{middle-button}, and \scheme{right-button}.
  ;* The last three match mouse buttons that are pressed.
  ;* \scheme{double} and \scheme{triple} are meaningful only for mouse button press
  ;* and release.
  ;* A key specifier is either a Scheme character, a range of characters
  ;* such as \scheme{#\a - #\z}, or a symbol naming one of the special keys,
  ;* such as
  ;* \scheme{escape}, 
  ;* \scheme{home}, 
  ;* \scheme{left}, 
  ;* \scheme{up}, 
  ;* \scheme{right}, 
  ;* \scheme{down}, 
  ;* \scheme{page_up}, 
  ;* \scheme{page_down}, 
  ;* \scheme{home}
  ;* \scheme{end},  etc.
  ;* The program \scheme{./apps/common/keyhelp.ss} can be helpful in finding
  ;* the symbolic name for a particular key.  Note that \scheme{alt} is a modifier
  ;* while \scheme{alt_l} and \scheme{alt_r} are keys.
  ;*
  ;* Each clause \scheme{(pattern e0 e1 ...)} is scanned in order until a
  ;* pattern is found that matches the event described by the \scheme{key=}
  ;* and \scheme{modifier=} keyword bindings.  If specified, the key must
  ;* match exactly.  A modifier matches if it contains at least the
  ;* modifiers specified by the pattern.  More specific patterns
  ;* should precede less specific patterns.
  ;* The \scheme{else} clause, if present, matches any event.
  ;* (See \ref{tutorial:canvas}.)
  ;*
  ;* Note that \scheme{key=} must be bound to a character or a value returned
  ;* by \scheme{keysym->keycode}, while \scheme{modifier=} must be bound to
  ;* a value returned by \scheme{add-modifiers} or \scheme{cancel-modifiers}.

  (lambda (x)
  
  ; skip this for now, but eventually write a pattern compiler
  ; to optimize placement of predicate tests...  (like char?)
  ;
  ;(define modifier-shadows?
  ;  (lambda (p1 p2)
  ;    (and (number? p1) (number? p2) (fx= (fxlogand p1 p2) p2))))
  ;
  ;(define pattern-shadows?
  ;  (lambda (p1 p2)
  ;    (or (modifier-shadows? p1 p2) (key-shadows? p1 p2))))
  
  ;; pattern comes in as list of disjuncts, we just check syntax and
  ;; turn pattern into list of disjuncts (the or's)
  ;; where each disjunct is
  ;;   modbitmap | key | (ascii . ascii) | (modbitmap . key) |
  ;;   (modbitmap ascii . ascii)
  
  (define parse-pattern
    (lambda (x)
      (cond
        ((null? x) (assertion-violationf 'event-case "empty pattern ()"))
        ((list? x)
          (map (parse-pat
                 (lambda () (assertion-violationf 'event-case "invalid pattern ~s" x)))
               x))
        (else (assertion-violationf 'event-case "invalid pattern ~s" x)))))

  (define make-pattern (lambda (what mods) (cons mods what)))
  (define pattern-mods car)
  (define pattern-keys cdr)
  
  (define parse-pat
    (lambda (err)
      (lambda (subpattern)
        (if (not (pair? subpattern))
            (err)
            (let parse ([x subpattern] [mods (add-modifiers)])
              (cond
                [(null? (cdr x))
                 (let ([fst (car x)])
                   (let ([mod (modifier->bitfield fst)])
                     (cond
                       [mod (make-pattern '() (add-modifiers mod mods))]
                       [(char? fst) (make-pattern fst mods)]
                       [else
                        (let ((x (keysym->keycode fst)))
                          (if x
                              (make-pattern x mods)
                              (assertion-violationf 'event-case
                                "invalid sub-pattern ~s"
                                subpattern)))])))]
                [(modifier->bitfield (car x))
                 =>
                 (lambda (mod) (parse (cdr x) (add-modifiers mod mods)))]
                [(char? (car x))
                 (unless (and (eq? (cadr x) '-)
                              (= (length x) 3)
                              (char? (caddr x))
                              (char<=? (car x) (caddr x)))
                   (assertion-violationf 'event-case "invalid character range ~s" x))
                 (make-pattern (cons (car x) (caddr x)) mods)]
                [else
                 (assertion-violationf 'event-case
                   "invalid component: ~s~n                     in sub-pattern: ~s"
                   (car x)
                   subpattern)]))))))
  
  (define pattern->pred
    (lambda (src key mod)
      (define shift?
        (lambda (mod)
          (fx= (add-modifiers shift)
               (fxlogand (add-modifiers shift) mod))))
      (define warn-check
        (lambda (c)
          (when (char-lower-case? c)
            (warningf 'event-case
               "shift modifier specified with lower-case key ~s" c))))
      (lambda (pat)
        (define do-keys
          (lambda (keys shift?)
            (cond
              ((null? keys) #f)
              ((pair? keys)
               (let ([x (car keys)] [y (cdr keys)])
                 (when shift? (warn-check x) (warn-check x))
                 (with-syntax ([x x] [y y] [key (key src)])
                   (syntax (and (char? key) (char<=? x key y))))))
              (else
               (when (and shift? (char? keys)) (warn-check keys))
               (with-syntax ([pat keys] [key (key src)])
                 (syntax (eq? key 'pat)))))))
          (define do-mods
            (lambda (mods)
              (if (fx= (add-modifiers) mods)
                  #f
                  (with-syntax ([pat mods] [mod (mod src)])
                    (syntax (fx= pat (fxlogand pat mod)))))))
        (let ([mods (pattern-mods pat)])
          (let ([mods (do-mods mods)]
                [keys (do-keys (pattern-keys pat) (shift? mods))])
            (if keys
                (if mods
                    (with-syntax ([keys keys] [mods mods])
                      (syntax (and keys mods)))
                    keys)
                (or mods (syntax #f))))))))
  
  (define build-event-case
    (lambda (key mod pats bods)
      (define mkerr
        (lambda (present? id what)
          (if present?
              (lambda (src) id)
              (lambda (src)
                (assertion-violationf 'event-case "~a required for clause ~s"
                  what (syntax-object->datum src))))))
      (let ([keybind (mkerr key (syntax k) "key")]
            [modbind (mkerr mod (syntax m) "modifier")])
        (with-syntax ([key key] [mod mod])
          (let ([clauses
                 (map (lambda (pat bod)
                        (syntax-case pat (else)
                          [else
                           (with-syntax ([bod bod]) (syntax (else . bod)))]
                          [otherwise
                           (with-syntax ([tests
                                          (map (pattern->pred
                                                 (cons pat bod) keybind modbind)
                                               (parse-pattern
                                                 (syntax-object->datum
                                                   pat)))]
                                         [bod bod])
                             (syntax ((or . tests) . bod)))]))
                      pats
                      bods)])
            (with-syntax ([clauses clauses])
              (syntax (let ([k key] [m mod]) (cond . clauses)))))))))
 

    (syntax-case x (key= modifier=)
      [(_ ((key= k) (modifier= mod)) (pat . body) ...)
       (build-event-case
         (syntax k) (syntax mod) (syntax (pat ...)) (syntax (body ...)))]
      [(_ ((key= k)) (pat . body) ...)
       (build-event-case
         (syntax k) #f (syntax (pat ...)) (syntax (body ...)))]
      [(_ ((modifier= mod)) (pat . body) ...)
       (build-event-case
         #f (syntax mod) (syntax (pat ...)) (syntax (body ...)))])))

;) ; end eval-when
