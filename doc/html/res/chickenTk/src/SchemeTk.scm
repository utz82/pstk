;(load "scheme_wish.scm")

;;
;; nl
;; string for newline
;;
(define nl (string #\newline))

;;
;; *tk-returns-proper-list*
;; if set to #t, eval-wish returns list
;; which is easier to use by scheme
;;
(define *tk-returns-proper-list* #f)

;;
;; *tk-is-running*
;; semaphore for event-loop
;;
(define *tk-is-running* #f)

;;
;; *tk-init-string*
;; some proc to communicate between Scheme and Tcl
;;
(define *tk-init-string* "
package require Tk

# --------------------------------

proc echo {args} {
    if {![winfo exists .debugoutput]} {
	toplevel .debugoutput
	pack [text .debugoutput.text]\\
	    -expand yes\\
	    -fill both
    }
    .debugoutput.text insert end $args\\n
    .debugoutput.text see end
}

# --------------------------------

namespace eval AutoName {
    variable c 0
    proc autoName {{result \\#\\#}} {
	variable c
	append result [incr c]
    }
    namespace export *
}
namespace import AutoName::*

proc callToScm {callKey args} {
    global scmVar
    set resultKey [autoName]
    puts \"(call \\#:$callKey \\\"$resultKey\\\" $args)\"
    flush stdout
    vwait scmVar($resultKey)
    set result $scmVar($resultKey)
    unset scmVar($resultKey)
    set result
}

proc evalCmdFromScm {cmd {properly 0}} {
    if {[catch {
	set result [uplevel \\#0 $cmd]
    } err]} {
	puts \"(error \\\"[string map [list \\\\ \\\\\\\\ \\\" \\\\\\\"] $err]\\\")\"
    } elseif $properly {
	puts \"(return [tclListToScmList $result])\"
    } else {
	puts \"(return \\\"[string map [list \\\\ \\\\\\\\ \\\" \\\\\\\"] $result]\\\")\"
    }
    flush stdout
}

proc tclListToScmList {l} {
    switch [llength $l] {
	0 {
	    return ()
	}
	1 {
	    if {[string range $l 0 0] eq \"\\#\"} {
		return $l
	    }
	    if {[regexp {^[0-9]+$} $l]} {
		return $l
	    }
	    if {[regexp {^[.[:alpha:]][^ ,\\\"\\'\\[\\]\\\\;]*$} $l]} {
		return $l
	    }
	    set result \\\"
	    append result\\
		[string map [list \\\" \\\\\\\" \\\\ \\\\\\\\] $l]
	    append result \\\"

	}
	default {
	    set result {}
	    foreach el $l {
		append result \" \" [tclListToScmList $el]
	    }
	    set result [string range $result 1 end]
	    return \"($result)\"
	}
    }
}

proc setVscrollbarIn {toplevel a b} {
    # set vertical scrollbar in $toplevel (managed by grid)
    if {$toplevel eq \".\"} {
	set scrollbar .vscroll
    } else {
	set scrollbar $toplevel.vscroll
    }
    if {($a == 0.0) && ($b == 1.0)} {
	set code \"catch {grid forget $scrollbar}\"
	after cancel $code
	after 100 $code
    } else {
	grid configure $scrollbar -row 0 -column 1 -sticky news
	set code [list $scrollbar set $a $b]
	after cancel $code
	after idle $code
    }
}

proc setHscrollbarIn {toplevel a b} {
    # set horizontal scrollbar in $toplevel (managed by grid)
    if {$toplevel eq \".\"} {
	set scrollbar .hscroll
    } else {
	set scrollbar $toplevel.hscroll
    }
    if {($a == 0.0) && ($b == 1.0)} {
	set code \"catch {grid forget $scrollbar}\"
	after cancel $code
	after 100 $code
    } else {
	grid configure $scrollbar -row 1 -column 0 -sticky news
	set code [list $scrollbar set $a $b]
	after cancel $code
	after idle $code
    }
}

namespace eval dummy {}

proc scrolledtext {f args} {
    frame $f -class Scrolledtext
    grid [eval text $f.text $args]\\
	[scrollbar $f.vscroll\\
	     -command [list $f.text yview]]\\
	-sticky news
    grid [scrollbar $f.hscroll\\
	      -orient horizontal\\
	      -command [list $f.text xview]]\\
	-sticky news
    grid rowconfigure $f 0 -weight 1
    grid columnconfigure $f 0 -weight 1
    $f.text configure\\
	-xscrollcommand [list setHscrollbarIn $f]\\
	-yscrollcommand [list setVscrollbarIn $f]
    bindtags $f.text [concat $f [bindtags $f.text]]
    rename $f ::dummy::$f
    proc $f {args} [subst {
	eval $f.text \\$args
    }]
    set f
}
")

;;
;; (eval-wish cmd)
;; extends Sven Hartrumpf's (wish ...) by handling results
;; halts on errors
;;
(define (eval-wish cmd)
  (wish (string-append
	 "evalCmdFromScm \""
	 (string-translate*
	  cmd
	  '(("\\" . "\\\\") ("\"" . "\\\"")))
	 "\""
	 (if *tk-returns-proper-list*
	     " 1"
	     "")))
  (let again ((result (read-wish)))
    (case (car result)
      ((return)
       (if *tk-returns-proper-list*
	   (evaluated-list-from-tcl (cadr result))
	   (cadr result)))
      ((call)
       (apply call-by-key (cdr result))
       (again (read-wish)))
      ((error)
       (error
	(string-append 
	 "occurred inside Tcl/Tk" nl " "
	 cmd nl " --> "
	 (cadr result))))
      (else 
       (error result)))))

;;
;; (tcl-list->scheme-list str)
;; converts string representing a proper Tcl/Tk list into a proper cons'd list
;;
(define (tcl-list->scheme-list str)
  (with-input-from-string 
      (eval-wish 
       (string-append "tclListToScmList {" str "}")) read))

;;
;; (evaluated-list-from-tcl l)
;; fine-tuned evaluation of returned tcl list
;; replaces event bindings by appropriate lambda bodies.
;;
(define (evaluated-list-from-tcl l)
  (cond
   ((pair? l)
    (cond ((equal? (car l) 'callToScm)
	   `(,(get-keyword
	       (string->keyword (->string (cadr l)))
	       commands-invoked-by-tk)
	     ,@(cddr l)))
	  (else (map evaluated-list-from-tcl l))))
   ((tk-id->widget l))
   (else l)))

;;
;; tk-ids+widgets
;; list of keys and widgets
;; (key1: #<widget1> key2: #<widget2> ...)
;;
(define tk-ids+widgets '())

;;
;; (tcl-true? obj)
;; returns #f if [string is false $obj]
;; returns #t otherwise
;;
(define tcl-true?
  (let ((false-values
	 `(0 "0" 'false "false" ,(string->symbol "0"))))
    (lambda (obj)
      (not (member obj false-values)))))

;;
;; (tk-id->widget id)
;; returns widget procedure corresponding to tk path, e. g. .g13.g15
;; or if it is not registered in tk-ids+widgets,
;; creates a new one if possible.
;;
(define (tk-id->widget id)
  (get-keyword
   (string->keyword (->string id)) 
   tk-ids+widgets
   (lambda ()
     (if (tcl-true? (tk/winfo 'exists id))
	 (make-widget-by-id (tk/winfo 'class id) (->string id))
	 #f))))

;;
;; tk-widgets: list of known procedures corresponding to Tk widgets 
;;
(define tk-widgets '())

;;
;; (widget? x)
;; important to know for widget options
;;
(define (widget? x)
  (and (memq x tk-widgets) #t))

;;
;; (tk-var varname)
;; returns option string sequence
;; intended for connections variable <-> radio button etc.
;;
(define (tk-var varname)
  (set-tk-var! varname "")
  (string-append "::scmVar(" (->string varname) ")"))

;;
;; (get-tk-var varname)
;; reads Tcl/Tk array variable $::scmVar(varname)
;; intended for connections variable <- radio button etc.
;;
(define (get-tk-var varname)
  (eval-wish
   (string-append "set ::scmVar(" (->string varname) ")")))

;;
;; (set-tk-var! varname value)
;; sets Tcl/Tk array variable $::scmVar(varname) to value
;; intended for connections variable -> radio button etc.
;;
(define (set-tk-var! varname value)
  (eval-wish
   (string-append
    "set ::scmVar(" (->string varname) ") {" (->string value) "}")))

;;
;; commands-invoked-by-tk: list of #:key1 cmd1 #:key2 cmd2 ...
;; so that (call-by-key #:keyi ...) can invoke cmdi.
;;
(define commands-invoked-by-tk '())

(define (call-by-key key resultvar . args)
  (let* ((cmd (get-keyword key commands-invoked-by-tk))
	 (result (apply cmd args))
	 (str
	  (if (equal? result "")
	      ""
	      (string-trimleft
	       (scheme-arglist->tk-argstring
		(list result))))))
    (set-tk-var! resultvar str)
    result))

;;
;; (make-widget-by-id 'toplevel ".")
;; low-level constructor
;;
(define make-widget-by-id
  (lambda (type id . options)
    (let* ((self #f)
	   (result
	    (lambda (command . args)
	      (case command
		((get-id) id)
		((create-widget)
		 ;;
		 ;; high-level constructor -- usage:
		 ;; (define text1 (#<widget> 'create-widget 'text))
		 ;;
		 (let* ((widget-type (->string (car args)))
			(id-prefix (if (string=? id ".") "" id))
 			(id-suffix 
 			 (->string (get-keyword #:widget-name args gensym)))
;			(id-suffix (->string (gensym)))
			(new-id
			 (string-append id-prefix "." id-suffix))
			(options (cdr args)))
		   (eval-wish
		    (string-append
		     widget-type " " new-id
		     (scheme-arglist->tk-argstring options)))
		   (apply make-widget-by-id
			  (append (list widget-type new-id) options))))
		((configure)
		 (cond
		  ((null? args)
		   (eval-wish
		    (string-append id " " (->string command))))
		  ((null? (cdr args))
		   (eval-wish
		    (string-append
		     id " " (->string command) 
		     (scheme-arglist->tk-argstring args))))
		  (else
		   (eval-wish
		    (string-append
		     id " " (->string command)
		     (scheme-arglist->tk-argstring args)))
		   (do ((args args (cddr args)))
		       ((null? args) '())
		     (let ((key (car args))
			   (val (cadr args)))
		       (cond 
			((null? options)
			 (set! options (list key val)))
			((not (memq key options))
			 (set! options (cons key (cons val options))))
			(else
			 (set-car! (cdr (memq key options)) val))))))))
		((cget)
		 (let ((key (car args)))
		   (get-keyword
		    key options
		    (lambda ()
		      (eval-wish
		       (string-append
			id " cget" (scheme-arglist->tk-argstring args)))))))
		((call exec)
		 ;;
		 ;; general commands (wm, winfo, etc.)
		 ;;
		 (eval-wish
		  (string-trimleft (scheme-arglist->tk-argstring args))))
		(else
		 ;;
		 ;; special commands (text, canvas, etc.)
		 ;;
		 (eval-wish
		  (string-append
		   id " " (->string command)
		   (scheme-arglist->tk-argstring args)))))
	      )))
      (set! tk-widgets (cons result tk-widgets))
      (set! tk-ids+widgets
	    (cons (string->keyword id) (cons result tk-ids+widgets)))
      (set! self result)
      result)))

;;
;; (scheme-arglist->tk-argstring args)
;; converts lisp-style arglist to tcl-style arglist
;;
(define (scheme-arglist->tk-argstring args)
  (apply string-append
	 (map (lambda (x)
		(cond 
		 ((eq? x #f) " 0")
		 ((eq? x #t) " 1")
		 ((eq? x '()) " {}")
		 ((keyword? x)
		  (string-append " -" (->string x)))
		 ((widget? x)
		  (string-append " " (x 'get-id)))
		 ((and (pair? x) (procedure? (car x)))
		  (let ((lambda-term (car x))
			(keystr (symbol->string (gensym)))
			(rest (cdr x)))
		    (set! commands-invoked-by-tk
			  (cons (string->keyword keystr)
				(cons lambda-term commands-invoked-by-tk)))
		    (string-append
		     " {callToScm "
		     keystr
		     (scheme-arglist->tk-argstring rest)
		     "}")
		    ))
		 ((procedure? x)
		  (scheme-arglist->tk-argstring `((,x))))
		 ((list? x)
		  (cond ((eq? (car x) '+)
			 ; bind .t <Destroy> {+cmd}
			 (let ((result
				(string-trimleft
				 (scheme-arglist->tk-argstring
				  (cdr x)))))
			   (cond ((string=? result "") " +")
				 ((string=? (substring result 0 1) "{")
				  (string-append 
				   " {+ "
				   (substring result 1)))
				 (else
				  (string-append " +" result)))))
			((and (= (length x) 3)
			      (equal? (car x) '@)
			      (number? (cadr x))
			      (number? (caddr x)))
			 ; (@ 4 19) -> " @4,19"
			 (string-append "@" (number->string (cadr x))
					"," (number->string (caddr x))))
			(else
			 (string-append 
			  " {"
			  (string-trimleft (scheme-arglist->tk-argstring x))
			  "}"))))
		 ((pair? x)
		  (string-append
		   " "
		   (->string (car x)) "." (->string (cdr x))))
		 ((string? x)
		  (if (string->number x)
		      (string-append " " x)
		      (string-append
		       " \""
		       (string-translate* x '(("\\" . "\\\\")
					      ("\"" . "\\\"")))
		       "\"")))
		 (else (string-append " " (->string x)))))
	      (tk-args-of args))))

;;
;; (tk-args-of arglist)
;; returns list where leading non-Tcl/Tk arguments are removed.
;;
(define (tk-args-of arglist)
  (do ((arglist arglist (cddr arglist)))
      ((or (null? arglist) 
	   (not (memq (car arglist)
			'(#:widget-name))))
       arglist)))

;;
;; (string-trimleft str)
;; cuts off leading blanks
;;
(define (string-trimleft str)
  (cond ((string=? str "") "")
	((string=? (substring str 0 1) " ")
	 (string-trimleft (substring str 1)))
	(else str)))

;;
;; tk
;; procedure associated with the root-window of Tk
;;
(define tk #f)
; (define tk (make-widget-by-id 'toplevel "." #:class wish))

;;
;; (start-tk)
;; starts wish
;; initialises some Tcl/Tk procedure and variable
;; initialises list of widget procedures
;; initialises list of commands invoked by Tk
;; binds <Destroy> of . to stop event-loop 
;;
(define (start-tk)
  (start-wish)
  (wish *tk-init-string*)
  (set! tk-ids+widgets '())
  (set! tk-widgets '())
  (set! tk (make-widget-by-id 'toplevel "." #:class 'Wish))
  (set! commands-invoked-by-tk '()))

(define (end-tk)
  (set! *tk-is-running* #f)
  (end-wish))

(define (event-loop)
  (tk/wm 'protocol tk 'WM_DELETE_WINDOW end-tk)
  (set! *tk-is-running* #t)
  (do ()
      ((not *tk-is-running*)
       (if *wish-output* 
	   (tk/wm 'protocol tk 'WM_DELETE_WINDOW '()))
       '())
    (let ((tk-statement (read-wish)))
      (if (and (list? tk-statement)
	       (eq? (car tk-statement) 'call))
	  (apply call-by-key (cdr tk-statement))))))

;;
;; (make-wish-func tkname)
;; temporary function to make tk calls
;;
(define (make-wish-func tkname)
  (let ((name (->string tkname)))
    (lambda args
      (eval-wish
       (string-append name (scheme-arglist->tk-argstring args))))))

;;
;; from Tcl
;;
(define tk/after (make-wish-func 'after))
(define tk/update (make-wish-func 'update))
;;
;; from Tk
;;
(define tk/clipboard (make-wish-func 'clipboard))
(define tk/bgerror (make-wish-func 'bgerror))
(define tk/bind (make-wish-func 'bind))
(define tk/bindtags (make-wish-func 'bindtags))
(define tk/destroy (make-wish-func 'destroy))
(define tk/event (make-wish-func 'event))
(define tk/focus (make-wish-func 'focus))
(define tk/grab (make-wish-func 'grab))
(define tk/grid (make-wish-func 'grid))
(define tk/image (make-wish-func 'image))
(define tk/lower (make-wish-func 'lower))
(define tk/option (make-wish-func 'option))
(define tk/pack (make-wish-func 'pack))
(define tk/place (make-wish-func 'place))
(define tk/raise (make-wish-func 'raise))
(define tk/selection (make-wish-func 'selection))
(define tk/winfo (make-wish-func 'winfo))
(define tk/wm (make-wish-func 'wm))

(define tk/choosecolor (make-wish-func "tk_chooseColor"))
(define tk/choosedirectory (make-wish-func "tk_chooseDirectory"))
(define tk/dialog (make-wish-func "tk_dialog"))
(define tk/getopenfile (make-wish-func "tk_getOpenFile"))
(define tk/getsavefile (make-wish-func "tk_getSaveFile"))
(define tk/messagebox (make-wish-func "tk_messageBox"))
(define tk/focusfollowsmouse (make-wish-func "tk_focusFollowsMouse"))
(define tk/focusnext (make-wish-func "tk_focusNext"))
(define tk/focusprev (make-wish-func "tk_focusPrev"))
(define tk/popup (make-wish-func "tk_popup"))

(define (tk/wait . args) (make-wish-func 'tkwait))

(define tk/appname (make-wish-func "tk appname"))
(define tk/caret (make-wish-func "tk caret"))
(define tk/scaling (make-wish-func "tk scaling"))
(define tk/useinputmethods (make-wish-func "tk useinputmethods"))
(define tk/windowingsystem (make-wish-func "tk windowingsystem"))

;;
;; (create-special-menu-in menu name . options)
;; creates special menus, 
;; e. g. Windows: .menu.system
;; e. g. Unix: .menu.help
;; e. g. Apple: .menu.apple
;;
(define (create-special-menu-in menu name . options)
  (apply make-widget-by-id 
	 `("menu"
	   ,(string-append (menu 'get-id) "." (->string name))
	   ,@options)))
