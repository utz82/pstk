;; PS/Tk -- A Portable Scheme Interface to the Tk GUI Toolkit
;; Copyright (c) 2019, Michael Neidel
;; Copyright (c) 2008, Kenneth A Dickey
;; Copyright (c) 2006-2008, Nils M Holm
;; Copyright (c) 2004, Wolf-Dieter Busch
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.
;;
;; PS/Tk is based on Chicken/Tk by Wolf-Dieter Busch (2004):
;; http://wolf-dieter-busch.de/html/Software/Tools/ChickenTk.htm
;; which is in turn based on Scheme_wish by Sven Hartrumpf (1997, 1998):
;; http://pi7.fernuni-hagen.de/hartrumpf/scheme_wish.scm
;;
;; These are the changes that I (Nils) made to turn Chicken/Tk into PS/Tk:
;;
;; - Removed all Chicken-isms except for PROCESS.
;; - All PS/Tk function names begin with TK/ or TK-:
;;     EVAL-WISH   --> TK-EVAL-WISH
;;     GET-TK-VAR  --> TK-GET-VAR
;;     SET-TK-VAR! --> TK-SET-VAR!
;;     START-TK    --> TK-START
;;     END-TK      --> TK-END
;;     EVENT-LOOP  --> TK-EVENT-LOOP
;; - Added TK-DISPATCH-EVENT.
;; - Added TK-WAIT-FOR-WINDOW because TK/WAIT returned too early.
;; - Removed some unused functions and variables.
;; - Replaced keyword lists with property lists.
;; - Removed ScrolledText compound widget.
;; - Removed :WIDGET-NAME option.
;; - Added a PLT Scheme version of RUN-PROGRAM.
;;
;; Contributions (in order of appearance):
;; - Jens Axel Soegaard: PLT Scheme/Windows RUN-PROGRAM.
;; - Taylor R Campbell: Scheme48 RUN-PROGRAM, portable GENSYM, and some R5RS
;;   portability fixes.
;; - Jeffrey T. Read: Gambit hacks (RUN-PROGRAM, keyword hack).
;; - Marc Feeley: Various versions of RUN-PROGRAM (Bigloo, Gauche, Guile,
;;   Kawa, Scsh, Stklos), SRFI-88 keyword auto-detection, some bug fixes.
;; - David St-Hilaire: suggested catching unspecific value in form->string.
;; - Ken Dickey: added Ikarus Scheme
;; - Ken Dickey: added Larceny Scheme
;; Thank you!
;;
;; Change Log:
;; 2020-06-22 Shut down application when pipe to Scheme process is broken,
;;            add an option to turn Tk errors into Scheme exceptions
;; 2019-06-26 Remove legacy keyword support detection, map panedwindow
;;            through ttk-map-widgets
;; 2019-02-26 Chicken 5 compatibility
;; 2019-02-26 Set default wish-program to tclsh8.6
;; 2010-07-03 Optional argument to 'start' for inputting name of wish/tclsh
;; 2010-06-30 Repackaged for Chicken, removing alternate run-program's.
;; 2008-06-22 Added Larceny Scheme support.
;; 2008-02-29 Added R6RS (Ikarus Scheme) support, added TTK/STYLE.
;; 2007-06-27 Renamed source file to pstk.scm.
;; 2007-06-27 Re-factored some large procedures, applied some cosmetics.
;; 2007-06-26 FORM->STRING catches unspecific values now, so event handlers
;;            no longer have to return specific values.
;; 2007-06-26 Re-imported the following ports from the processio/v1 snowball:
;;            Bigloo, Gauche, Guile, Kawa, Scsh, Stklos.
;; 2007-06-26 Added auto-detection of SRFI-88 keywords.
;; 2007-03-03 Removed callback mutex, because it blocked some redraw
;;            operations. Use TK-WITH-LOCK to protect critical sections.
;; 2007-02-03 Added Tile support: TTK-MAP-WIDGETS, TTK/AVAILABLE-THEMES,
;;            TTK/SET-THEME.
;; 2007-01-20 Added (Petite) Chez Scheme port.
;; 2007-01-06 Fix: TK-WAIT-FOR-WINDOW requires nested callbacks.
;; 2007-01-05 Added code to patch through fatal TCL messages.
;; 2007-01-05 Protected call-backs by a mutex, so accidental double
;;            clicks, etc cannot mess up program state.
;; 2006-12-21 Made FORM->STRING accept '().
;; 2006-12-18 Installing WM_DELETE_WINDOW handler in TK-START now, so it does
;;            not get reset in TK-EVENT-LOOP.
;; 2006-12-18 Made TK-START and TK-END return () instead of #<unspecific>
;;            (which crashes FORM->STRING).
;; 2006-12-12 Fixed some wrong Tcl quotation (introduced by myself).
;; 2006-12-09 Added TK/BELL procedure.
;; 2006-12-08 Replaced ATOM->STRING by FORM->STRING.
;; 2006-12-06 Added TK-WAIT-UNTIL-VISIBLE.
;; 2006-12-03 Made more variables local to outer LETREC.
;; 2006-12-03 Added Gambit port and keywords hack.
;; 2006-12-02 Added Scheme 48 port, portable GENSYM, R5RS fixes.
;; 2006-12-02 Added PLT/Windows port.

(module pstk
  (tk
   tk-throw-exceptions
   tk-dispatch-event
   tk-end
   tk-eval
   tk-event-loop
   tk-get-var
   tk-id->widget
   tk-set-var!
   tk-start
   tk-var
   tk-wait-for-window
   tk-wait-until-visible
   tk-with-lock
   tk/after
   tk/appname
   tk/bell
   tk/bgerror
   tk/bind
   tk/bindtags
   tk/caret
   tk/choose-color
   tk/choose-directory
   tk/clipboard
   tk/destroy
   tk/dialog
   tk/event
   tk/focus
   tk/focus-follows-mouse
   tk/focus-next
   tk/focus-prev
   tk/font
   tk/get-open-file
   tk/get-save-file
   tk/grab
   tk/grid
   tk/image
   tk/lower
   tk/message-box
   tk/option
   tk/pack
   tk/place
   tk/popup
   tk/raise
   tk/scaling
   tk/selection
   tk/update
   tk/useinputmethods
   tk/wait
   tk/windowingsystem
   tk/winfo
   tk/wm
   ttk-map-widgets
   ttk/available-themes
   ttk/set-theme
   ttk/style
   )

  (import scheme)
  (cond-expand
    (chicken-4
     (import chicken posix)
     (use posix
	  (only data-structures string-intersperse)
	  (only srfi-13 string-concatenate)))
    (chicken-5
     (import chicken.base chicken.keyword chicken.file.posix chicken.process
	     chicken.string scheme srfi-1 srfi-13)))

  (define *wish-program* "tclsh8.6")
  (define *wish-debug-input* #f)
  (define *wish-debug-output* #f)

  (define tk #f)
  (define tk-dispatch-event #f)
  (define tk-end #f)
  (define tk-eval #f)
  (define tk-event-loop #f)
  (define tk-get-var #f)
  (define tk-id->widget #f)
  (define tk-set-var! #f)
  (define tk-start #f)
  (define tk-var #f)
  (define tk-wait-for-window #f)
  (define tk-wait-until-visible #f)
  (define tk-with-lock #f)
  (define tk/after #f)
  (define tk/appname #f)
  (define tk/bell #f)
  (define tk/bgerror #f)
  (define tk/bind #f)
  (define tk/bindtags #f)
  (define tk/caret #f)
  (define tk/choose-color #f)
  (define tk/choose-directory #f)
  (define tk/clipboard #f)
  (define tk/destroy #f)
  (define tk/dialog #f)
  (define tk/event #f)
  (define tk/focus #f)
  (define tk/focus-follows-mouse #f)
  (define tk/focus-next #f)
  (define tk/focus-prev #f)
  (define tk/font #f)
  (define tk/get-open-file #f)
  (define tk/get-save-file #f)
  (define tk/grab #f)
  (define tk/grid #f)
  (define tk/image #f)
  (define tk/lower #f)
  (define tk/message-box #f)
  (define tk/option #f)
  (define tk/pack #f)
  (define tk/place #f)
  (define tk/popup #f)
  (define tk/raise #f)
  (define tk/scaling #f)
  (define tk/selection #f)
  (define tk/update #f)
  (define tk/useinputmethods #f)
  (define tk/wait #f)
  (define tk/windowingsystem #f)
  (define tk/winfo #f)
  (define tk/wm #f)
  (define ttk-map-widgets #f)
  (define ttk/available-themes #f)
  (define ttk/set-theme #f)
  (define ttk/style #f)

  (define tk-throw-exceptions
    (let ((enabled #f))
      (lambda args
	(if (null? args)
	    enabled
	    (set! enabled (car args))))))

  (letrec

    ((nl (string #\newline))

     (wish-input #f)

     (wish-output #f)

     (tk-is-running #f)

     (tk-ids+widgets '())

     (tk-widgets '())

     (commands-invoked-by-tk '())

     (inverse-commands-invoked-by-tk '())

     (in-callback #f)

     (callback-mutex #t)

     (ttk-widget-map '())

     (tk-init-string
       (string-intersperse
                          '("package require Tk"
                            "if {[package version tile] != \"\"} {"
                            "    package require tile"
                            "}"
                            ""
                            "namespace eval AutoName {"
                            "    variable c 0"
                            "    proc autoName {{result \\#\\#}} {"
                            "        variable c"
                            "        append result [incr c]"
                            "    }"
                            "    namespace export *"
                            "}"
                            ""
                            "namespace import AutoName::*"
                            ""
                            "proc callToScm {callKey args} {"
                            "    global scmVar"
			    "    if { [catch {"
                            "            set resultKey [autoName]"
                            "            puts \"(call $callKey \\\"$resultKey\\\" $args)\""
                            "            flush stdout"
                            "            vwait scmVar($resultKey)"
                            "            set result $scmVar($resultKey)"
                            "            unset scmVar($resultKey)"
                            "            set result"
			    "         }]} { exit 1 }"
                            "}"
                            ""
                            "proc tclListToScmList {l} {"
                            "    switch [llength $l] {"
                            "        0 {"
                            "            return ()"
                            "        }"
                            "        1 {"
                            "            if {[string range $l 0 0] eq \"\\#\"} {"
                            "                return $l"
                            "            }"
                            "            if {[regexp {^[0-9]+$} $l]} {"
                            "                return $l"
                            "            }"
                            "            if {[regexp {^[.[:alpha:]][^ ,\\\"\\'\\[\\]\\\\;]*$} $l]} {"
                            "                return $l"
                            "            }"
                            "            set result \\\""
                            "            append result\\"
                            "                [string map [list \\\" \\\\\\\" \\\\ \\\\\\\\] $l]"
                            "            append result \\\""
                            ""
                            "        }"
                            "        default {"
                            "            set result {}"
                            "            foreach el $l {"
                            "                append result \" \" [tclListToScmList $el]"
                            "            }"
                            "            set result [string range $result 1 end]"
                            "            return \"($result)\""
                            "        }"
                            "    }"
                            "}"
                            ""
                            "proc evalCmdFromScm {cmd {properly 0}} {"
                            "    if {[catch {"
                            "        set result [uplevel \\#0 $cmd]"
                            "    } err]} {"
                            "        puts \"(error \\\"[string map [list \\\\ \\\\\\\\ \\\" \\\\\\\"] $err]\\\")\""
                            "    } elseif $properly {"
                            "        puts \"(return [tclListToScmList $result])\""
                            "    } else {"
                            "        puts \"(return \\\"[string map [list \\\\ \\\\\\\\ \\\" \\\\\\\"] $result]\\\")\""
                            "    }"
                            "    flush stdout"
                            "}")
                          (string #\newline)))

     (report-error
      (lambda (x)
        (newline)
        (display x)
        (newline)
	(when (tk-throw-exceptions) (error 'tk (->string x)))))

     (run-program
       (lambda (program)
         (call-with-values
           (lambda ()
             (process program))
           (lambda (in out pid)
             (list in out)))))

     (flush-output-port flush-output)

     (flush-wish
       (lambda ()
         (flush-output-port wish-input)))

     (option?
       (lambda (x)
         (or (keyword? x)
             (and (symbol? x)
                  (let* ((s (symbol->string x))
                         (n (string-length s)))
                    (char=? #\: (string-ref s (- n 1))))))))

     (make-option-string
       (lambda (x)
         (if (keyword? x)
           (string-append " -" (keyword->string x))
           (let ((s (symbol->string x)))
             (string-append " -"
                            (substring s 0 (- (string-length s) 1)))))))

     (improper-list->string
       (lambda (a first)
         (cond ((pair? a)
                (cons (string-append (if first "" " ")
                                     (form->string (car a)))
                      (improper-list->string (cdr a) #f)))
               ((null? a) '())
               (else (list (string-append " . " (form->string a)))))))

     (form->string
       (lambda (x)
         (cond ((eq? #t x) "#t")
               ((eq? #f x) "#f")
               ((number? x) (number->string x))
               ((symbol? x) (symbol->string x))
               ((string? x) x)
               ((null? x) "()")
               ((pair? x)
                (string-append "("
                               (string-concatenate
                                      (improper-list->string x #t))
                               ")"))
               ((eof-object? x) "#<eof>")
               (else "#<other>"))))

     (string-translate
       (lambda (s map)
         (letrec
           ((s-prepend (lambda (s1 s2)
                         (cond ((null? s1) s2)
                               (else (s-prepend (cdr s1) (cons (car s1) s2))))))
            (s-xlate (lambda (s r)
                       (cond ((null? s) (reverse r))
                             (else (let ((n (assv (car s) map)))
                                     (cond (n (s-xlate (cdr s)
                                                       (s-prepend (string->list (cdr n)) r)))
                                           (else (s-xlate (cdr s)
                                                          (cons (car s) r))))))))))
           (list->string
             (s-xlate (string->list s) '())))))

     (string-trim-left
       (lambda (str)
         (cond ((string=? str "") "")
               ((string=? (substring str 0 1) " ")
                (string-trim-left (substring str 1
                                             (string-length str))))
               (else str))))

     (get-property
       (lambda (key args . thunk)
         (cond ((null? args)
                (cond ((null? thunk) #f)
                      (else ((car thunk)))))
               ((eq? key (car args))
                (cond ((pair? (cdr args)) (cadr args))
                      (else (report-error (list 'get-property key args)))))
               ((or (not (pair? (cdr args)))
                    (not (pair? (cddr args))))
                (report-error (list 'get-property key args)))
               (else (apply get-property key (cddr args) thunk)))))

     (tcl-true?
       (let ((false-values
               `(0 "0" 'false "false" ,(string->symbol "0"))))
         (lambda (obj) (not (memv obj false-values)))))

     (widget?
       (lambda (x)
         (and (memq x tk-widgets) #t)))

     (call-by-key
       (lambda (key resultvar . args)
         (cond ((and in-callback (pair? callback-mutex)) #f)
               (else (set! in-callback (cons #t in-callback))
                     (let* ((cmd (get-property key commands-invoked-by-tk))
                            (result (apply cmd args))
                            (str (string-trim-left
                                   (scheme-arglist->tk-argstring
                                     (list result)))))
                       (set-var! resultvar str)
                       (set! in-callback (cdr in-callback))
                       result)))))

     (gen-symbol
       (let ((counter 0))
         (lambda ()
           (let ((sym (string-append "g" (number->string counter))))
             (set! counter (+ counter 1))
             (string->symbol sym)))))

     (widget-name
       (lambda (x)
         (let ((name (form->string x)))
           (cond ((member name ttk-widget-map)
                  (string-append "ttk::" name))
                 (else name)))))

     (make-widget-by-id
       (lambda (type id . options)
         (let
           ((result
              (lambda (command . args)
                (case command
                  ((get-id) id)
                  ((create-widget)
                   (let* ((widget-type (widget-name (car args)))
                          (id-prefix (if (string=? id ".") "" id))
                          (id-suffix (form->string (gen-symbol)))
                          (new-id (string-append id-prefix "." id-suffix))
                          (options (cdr args)))
                     (eval-wish
                       (string-append
                         widget-type
                         " "
                         new-id
                         (scheme-arglist->tk-argstring options)))
                     (apply make-widget-by-id
                            (append (list widget-type new-id)
                                    options))))
                  ((configure)
                   (cond ((null? args)
                          (eval-wish
                            (string-append id " " (form->string command))))
                         ((null? (cdr args))
                          (eval-wish
                            (string-append
                              id
                              " "
                              (form->string command)
                              (scheme-arglist->tk-argstring args))))
                         (else
                           (eval-wish
                             (string-append
                               id
                               " "
                               (form->string command)
                               (scheme-arglist->tk-argstring args)))
                           (do ((args args (cddr args)))
                             ((null? args) '())
                             (let ((key (car args)) (val (cadr args)))
                               (cond ((null? options)
                                      (set! options (list key val)))
                                     ((not (memq key options))
                                      (set! options
                                        (cons key (cons val options))))
                                     (else (set-car! (cdr (memq key options))
                                                     val))))))))
                  ((cget)
                   (let ((key (car args)))
                     (get-property
                       key
                       options
                       (lambda ()
                         (eval-wish
                           (string-append
                             id
                             " cget"
                             (scheme-arglist->tk-argstring args)))))))
                  ((call exec)
                   (eval-wish
                     (string-trim-left
                       (scheme-arglist->tk-argstring args))))
                  (else
                    (eval-wish
                      (string-append
                        id
                        " "
                        (form->string command)
                        (scheme-arglist->tk-argstring args))))))))
           (set! tk-widgets (cons result tk-widgets))
           (set! tk-ids+widgets
             (cons (string->symbol id)
                   (cons result tk-ids+widgets)))
           result)))

     (scheme-arg->tk-arg
       (lambda (x)
         (cond ((eq? x #f) " 0")
               ((eq? x #t) " 1")
               ((eq? x '()) " {}")
               ((option? x) (make-option-string x))
               ((widget? x) (string-append " " (x 'get-id)))
               ((and (pair? x) (procedure? (car x)))
                (let* ((lambda-term (car x))
                       (rest (cdr x))
                       (l (memq lambda-term
                                inverse-commands-invoked-by-tk))
                       (keystr (if l (form->string (cadr l))
                                 (symbol->string (gen-symbol)))))
                  (if (not l)
                    (let ((key (string->symbol keystr)))
                      (set! inverse-commands-invoked-by-tk
                        (cons lambda-term
                              (cons key
                                    inverse-commands-invoked-by-tk)))
                      (set! commands-invoked-by-tk
                        (cons key
                              (cons lambda-term
                                    commands-invoked-by-tk)))))
                  (string-append " {callToScm "
                                 keystr
                                 (scheme-arglist->tk-argstring rest)
                                 "}")))
               ((procedure? x)
                (scheme-arglist->tk-argstring `((,x))))
               ((list? x)
                (cond ((eq? (car x) '+)
                       (let ((result (string-trim-left
                                       (scheme-arglist->tk-argstring
                                         (cdr x)))))
                         (cond ((string=? result "") " +")
                               ((string=? "{" (substring result 0 1))
                                (string-append
                                  " {+ "
                                  (substring result 1
                                             (string-length result))))
                               (else (string-append " +" result)))))
                      ((and (= (length x) 3)
                            (equal? (car x) (string->symbol "@"))
                            (number? (cadr x))
                            (number? (caddr x)))
                       (string-append
                         "@"
                         (number->string (cadr x))
                         ","
                         (number->string (caddr x))))
                      (else
                        (string-append
                          " {"
                          (string-trim-left
                            (scheme-arglist->tk-argstring x))
                          "}"))))
               ((pair? x)
                (string-append
                  " "
                  (form->string (car x))
                  "."
                  (form->string (cdr x))))
               ((string? x)
                (if (string->number x)
                  (string-append " " x)
                  (string-append
                    " \""
                    (string-translate x
                                      '((#\\ . "\\\\") (#\" . "\\\"")
                                                       (#\[ . "\\u005b") (#\] . "\\]")
                                                       (#\$ . "\\u0024")
                                                       (#\{ . "\\{") (#\} . "\\}")))
                    "\"")))
               (else (string-append " " (form->string x))))))

     (scheme-arglist->tk-argstring
       (lambda (args)
         (string-concatenate
                (map scheme-arg->tk-arg
                     args))))

     (make-wish-func
       (lambda (tkname)
         (let ((name (form->string tkname)))
           (lambda args
             (eval-wish
               (string-append
                 name
                 (scheme-arglist->tk-argstring args)))))))

     (read-wish
       (lambda ()
         (let ((term (read wish-output)))
           (cond (*wish-debug-output*
                   (display "wish->scheme: ")
                   (write term)
                   (newline)))
           term)))

     (wish
       (lambda arguments
         (for-each
           (lambda (argument)
             (cond (*wish-debug-input*
                     (display "scheme->wish: ")
                     (display argument)
                     (newline)))
             (display argument wish-input)
             (newline wish-input)
             (flush-wish))
           arguments)))

     (start-wish
       (lambda ()
         (let ((result (run-program *wish-program*)))
           (set! wish-input (cadr result))
           (set! wish-output (car result)))))

     (read-line
       (lambda (in)
         (letrec
           ((collect-chars
              (lambda (c s)
                (cond ((or (eof-object? c) (char=? c #\newline))
                       (reverse-list->string s))
                      (else (collect-chars (read-char in) (cons c s))))))
            (first-char
              (read-char in)))
           (cond ((eof-object? first-char) first-char)
                 (else (collect-chars first-char '()))))))

     (eval-wish
       (lambda (cmd)
         (wish (string-append
                 "evalCmdFromScm \""
                 (string-translate cmd
                                   '((#\\ . "\\\\") (#\" . "\\\"")))
                 "\""))
         (let again ((result (read-wish)))
           (cond ((not (pair? result))
                  (report-error (string-append
                                  "An error occurred inside Tcl/Tk" nl
                                  " --> " (form->string result)
                                  " " (read-line wish-output))))
                 ((eq? (car result) 'return)
                  (cadr result))
                 ((eq? (car result) 'call)
                  (apply call-by-key (cdr result))
                  (again (read-wish)))
                 ((eq? (car result) 'error)
                  (report-error (string-append
                                  "An error occurred inside Tcl/Tk" nl
                                  " " cmd nl
                                  " --> " (cadr result))))
                 (else (report-error result))))))

     (id->widget
       (lambda (id)
         (get-property
           (string->symbol (form->string id))
           tk-ids+widgets
           (lambda ()
             (if (tcl-true? (tk/winfo 'exists id))
               (make-widget-by-id
                 (tk/winfo 'class id)
                 (form->string id))
               #f)))))

     (var
       (lambda (varname)
         (set-var! varname "")
         (string-append
           "::scmVar("
           (form->string varname)
           ")")))

     (get-var
       (lambda (varname)
         (eval-wish
           (string-append
             "set ::scmVar("
             (form->string varname)
             ")"))))

     (set-var!
       (lambda (varname value)
         (eval-wish
           (string-append
             "set ::scmVar("
             (form->string varname)
             ") {"
             (form->string value)
             "}"))))

     (start
       (lambda args ; optional argument allows user to input name of wish program
         (if (and (not (null? args)) (= 1 (length args)))
           (set! *wish-program* (car args)))
         (start-wish)
         (wish tk-init-string)
         (set! tk-ids+widgets '())
         (set! tk-widgets '())
         (set! in-callback #f)
         (set! tk (make-widget-by-id 'toplevel "." 'class: 'Wish))
         (set! commands-invoked-by-tk '())
         (set! inverse-commands-invoked-by-tk '())
         (tk/wm 'protocol tk 'WM_DELETE_WINDOW end-tk)))

     (end-tk
       (lambda ()
         (set! tk-is-running #f)
         (wish "after 200 exit")))

     (dispatch-event
       (lambda ()
         (let ((tk-statement (read-wish)))
           (if (and (list? tk-statement)
                    (eq? (car tk-statement) 'call))
             (apply call-by-key (cdr tk-statement))))))

     (loop
       (lambda ()
         (cond ((not tk-is-running)
                (if wish-output
                  (tk/wm 'protocol tk 'WM_DELETE_WINDOW '())))
               (else (dispatch-event)
                     (loop)))))

     (event-loop
       (lambda ()
         (set! tk-is-running #t)
         (loop)))

     (map-ttk-widgets
       (lambda (x)
         (cond ((eq? x 'all)
                (set! ttk-widget-map '("button" "checkbutton" "radiobutton"
                                       "menubutton" "label" "entry" "frame"
                                       "labelframe" "scrollbar" "notebook"
				       "panedwindow"
                                       "progressbar" "combobox" "separator"
                                       "scale" "sizegrip" "treeview")))
               ((eq? x 'none)
                (set! ttk-widget-map '()))
               ((pair? x) (set! ttk-widget-map
                            (map form->string x)))
               (else (report-error
                       (string-append
                         "Argument to TTK-MAP-WIDGETS must be "
                         "ALL, NONE or a list of widget types."))))))

     (string-split
       (lambda (c s)
         (letrec
           ((split (lambda (i k tmp res)
                     (cond ((= i k)
                            (if (null? tmp) res (cons tmp res)))
                           ((char=? (string-ref s i) c)
                            (split (+ i 1) k "" (cons tmp res)))
                           (else (split (+ i 1) k
                                        (string-append tmp
                                                       (string (string-ref s i)))
                                        res))))))
           (reverse (split 0 (string-length s) "" '())))))

     (ttk-available-themes
       (lambda ()
         (string-split #\space (eval-wish "ttk::style theme names"))))

     (do-wait-for-window
       (lambda (w)
         (dispatch-event)
         (cond ((equal? (tk/winfo 'exists w) "0") '())
               (else (do-wait-for-window w)))))

     (wait-for-window
       (lambda (w)
         (let ((outer-allow callback-mutex))
           (set! callback-mutex #t)
           (do-wait-for-window w)
           (set! callback-mutex outer-allow))))

     (wait-until-visible
       (lambda (w)
         (tk/wait 'visibility w)))

     (lock!
       (lambda ()
         (set! callback-mutex
           (cons callback-mutex #t))))

     (unlock!
       (lambda ()
         (if (pair? callback-mutex)
           (set! callback-mutex
             (cdr callback-mutex)))))

     (with-lock
       (lambda (thunk)
         (lock!)
         (thunk)
         (unlock!))))

    (set! tk-eval eval-wish)
    (set! tk-id->widget id->widget)
    (set! tk-var var)
    (set! tk-get-var get-var)
    (set! tk-set-var! set-var!)
    (set! tk-start start)
    (set! tk-end end-tk)
    (set! tk-dispatch-event dispatch-event)
    (set! tk-event-loop event-loop)
    (set! tk-wait-for-window wait-for-window)
    (set! tk-wait-until-visible wait-until-visible)
    (set! tk-with-lock with-lock)
    (set! tk/after (make-wish-func 'after))
    (set! tk/bell (make-wish-func 'bell))
    (set! tk/update (make-wish-func 'update))
    (set! tk/clipboard (make-wish-func 'clipboard))
    (set! tk/bgerror (make-wish-func 'bgerror))
    (set! tk/bind (make-wish-func 'bind))
    (set! tk/bindtags (make-wish-func 'bindtags))
    (set! tk/destroy (make-wish-func 'destroy))
    (set! tk/event (make-wish-func 'event))
    (set! tk/focus (make-wish-func 'focus))
    (set! tk/font (make-wish-func 'font))
    (set! tk/grab (make-wish-func 'grab))
    (set! tk/grid (make-wish-func 'grid))
    (set! tk/image (make-wish-func 'image))
    (set! tk/lower (make-wish-func 'lower))
    (set! tk/option (make-wish-func 'option))
    (set! tk/pack (make-wish-func 'pack))
    (set! tk/place (make-wish-func 'place))
    (set! tk/raise (make-wish-func 'raise))
    (set! tk/selection (make-wish-func 'selection))
    (set! tk/winfo (make-wish-func 'winfo))
    (set! tk/wm (make-wish-func 'wm))
    (set! tk/choose-color (make-wish-func "tk_chooseColor"))
    (set! tk/choose-directory (make-wish-func "tk_chooseDirectory"))
    (set! tk/dialog (make-wish-func "tk_dialog"))
    (set! tk/get-open-file (make-wish-func "tk_getOpenFile"))
    (set! tk/get-save-file (make-wish-func "tk_getSaveFile"))
    (set! tk/message-box (make-wish-func "tk_messageBox"))
    (set! tk/focus-follows-mouse (make-wish-func "tk_focusFollowsMouse"))
    (set! tk/focus-next (make-wish-func "tk_focusNext"))
    (set! tk/focus-prev (make-wish-func "tk_focusPrev"))
    (set! tk/popup (make-wish-func "tk_popup"))
    (set! tk/wait (lambda args (make-wish-func 'tkwait)))
    (set! tk/appname (make-wish-func "tk appname"))
    (set! tk/caret (make-wish-func "tk caret"))
    (set! tk/scaling (make-wish-func "tk scaling"))
    (set! tk/useinputmethods (make-wish-func "tk useinputmethods"))
    (set! tk/windowingsystem (make-wish-func "tk windowingsystem"))
    (set! ttk/available-themes ttk-available-themes)
    (set! ttk/set-theme (make-wish-func "ttk::style theme use"))
    (set! ttk/style (make-wish-func "ttk::style"))
    (set! ttk-map-widgets map-ttk-widgets))
  )
