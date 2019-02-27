;; scheme_wish: A portable interface between Scheme and Tcl/Tk.
;; It is based on the idea to start a wish process from the Scheme process and
;; to communicate using pipes.
;; scheme_wish has been tested for the following Scheme systems:
;; Gambit, guile, and SCM.
;; version 0.1 - 1998-04-26
;; The current version is available from my home page
;; (http://www.informatik.fernuni-hagen.de/pi7/hartrumpf/).
;; I supply this file without any guarantees and for noncommercial use only.

;; Sven Hartrumpf (C) 1997-1998
;; Applied Computer Science VII, FernUniversität Hagen, 58084 Hagen, Germany
;; email: Sven.Hartrumpf@FernUni-Hagen.de

;; A small example is given in function test-wish.
;; To test it, do the following:
;; 1. Change the six global constants below (if necessary).
;; 2. Uncomment the necessary system-dependent part at the end of this file.
;; 3. Start Scheme.
;; 4. Load this file into Scheme.
;; 5. Type: (test-wish).


;; to do list
;;  - solve bigloo problem


;; global constants
;; One might have to adjust the following six definitions.

;; A true value causes all data that flows from Scheme to wish to be written on
;; standard output by Scheme.
(define *wish-debug-input* #f)

;; A true value causes all data that flows from wish to Scheme to be written on
;; standard output by Scheme.
(define *wish-debug-output* #f)

;; The message that will be send to the wish process to close it.
(define *wish-exit-message* "after 200 exit")

;; The name of the file that will become the input of the wish process.
(define *wish-input-filename* "/tmp/scheme_wish_i")

;; The name of the file that will become the output of the wish process.
(define *wish-output-filename* "/tmp/scheme_wish_o")

;; The name of the shell command that starts a wish process.
(define *wish-program* "wish")

;; global variables

; (define *wish-input* #f) ; The input stream of the wish process.

; (define *wish-output* #f) ; The output stream of the wish process.

;; functions
;;   relevant high level functions: start-wish, wish, read-wish, end-wish


;; (end-wish) = unspecified
;; Side effect: terminates the wish process.

(define end-wish
  (lambda ()
    (wish *wish-exit-message*)))

;; (flush-wish) = unspecified
;; Flushes the output to wish.

(define flush-wish
  (lambda ()
    (flush-output *wish-input*)))

;; (read-wish) =
;;   reads a term from wish.

(define read-wish
  (lambda ()
    (let ((term (read *wish-output*)))
      (cond (*wish-debug-output*
	     (display "wish->scheme: ")
	     (write term)
	     (newline)))
      term)))

;; (read-wish-chars) =
;;   a line from wish as a list of characters.

(define read-wish-chars
  (lambda ()
    (read-bytes-until *wish-output* '(#\newline))))

(define read-bytes-until
  (lambda (port stop-characters)
    (let ((char (read-char port)))
      (cond ((eof-object? char)
	     '())
	    ((memq char stop-characters)
	     '())
	    (else
	     (cons char (read-bytes-until port stop-characters)))))))

;; (run-program p)
;; Side effect: runs the program p with input and output bound to streams.

; (define run-program
;   (lambda (program input-filename output-filename)
;     (let ((command #f)
; 	  (input #f)
; 	  (output #f)
; 	  (result #f))
;       (delete-file-if-exists input-filename)
;       ;;(write "input file deleted")(newline)
;       (delete-file-if-exists output-filename)
;       ;;(write "output file deleted")(newline)
;       (set! result (system (string-append "mknod " input-filename " p")))
;       ;;(write result)(newline)
;       (set! result (system (string-append "mknod " output-filename " p")))
;       ;;(write result)(newline)
;       (set! command (string-append program " < " 
; 				   input-filename " > "
; 				   output-filename " 2>&1"))
;       ;; 2>&1 associates file descriptor 2 (standard error) with the file associated
;       ;; with file descriptor 1.
;       (set! result (system (string-append "/bin/sh -c \"" command "\" &")))
;       ;;(write result)(newline)
;       (set! input (open-output-file input-filename))
;       ;;(write input)(newline)
;       (set! output (open-input-file output-filename))
;       ;;(write output)(newline)
;       (list input output))))

;; start-wish
;; Starts a wish process.

; (define start-wish
;   (lambda ()
;     (let ((result (run-program *wish-program* *wish-input-filename* *wish-output-filename*)))
;       (set! *wish-input* (car result))
;       (set! *wish-output* (cadr result)))))


;; test-wish
;; A simple test program.

(define test-wish
  (lambda ()
    (start-wish)

    ;; Tcl procedures

    ;; scm is used to send something from wish to Scheme.
    (w "
proc scm {s} {
  puts $s
  flush stdout
}
")

    ;; widget definitions

    (w
     "
label .l -text \"scheme_wish: A portable interface between Scheme and Tcl/Tk; version 0.1; written by Sven Hartrumpf\"
frame .alternative
radiobutton .alternative.a -borderwidth 2 -relief raised -pady 4 -text \"A\" -value a -variable alternative -command {scm \"(alternative-value $alternative)\"} -foreground #0000af
radiobutton .alternative.b -borderwidth 2 -relief raised -pady 4 -text \"B\" -value b -variable alternative -command {scm \"(alternative-value $alternative)\"} -foreground #2060ff
radiobutton .alternative.c -borderwidth 2 -relief raised -pady 4 -text \"C\" -value c -variable alternative -command {scm \"(alternative-value $alternative)\"} -foreground #60d0ff
button .end -text \"Quit\" -command {scm \"(quit)\"}
")

    ;; widget placements
    (w "
pack .alternative.a .alternative.b .alternative.c -expand yes -fill x -side left
pack .l .alternative .end -expand yes -fill both -side top
")

    (test-wish-handler)
    (end-wish)
    ))

(define test-wish-handler
  (lambda ()
    (do ((alternative #f)
	 (end #f))
	(end alternative)
      (let ((term (read-wish)))
	(display "test-wish-handler: term: ")
	(write term)
	(newline)
	(cond ((pair? term)
	       (case (car term)
		 ((alternative-value)
		  (set! alternative (cadr term))
		  (display "alternative: ")
		  (write alternative)
		  (newline))
		 ((quit)
		  (set! end #t))
		 (else
		  (display "test-wish-handler: unexpected term: ")
		  (write term)
		  (newline))))
	      ((eof-object? term)
	       (display "eof object received")
	       (newline)
	       (set! end #t))
	      (else
	       (display "test-wish-handler: unexpected term: ")
	       (write term)
	       (newline)))))))


;; (wish . l)
;; Sends all arguments to wish's input using display.

(define wish
  (lambda arguments
    (for-each
     (lambda (argument)
       (cond (*wish-debug-input*
	      (display "scheme->wish: ")
	      (display argument)
	      (newline)))
       (display argument *wish-input*)
       (newline *wish-input*))
     arguments)
    (flush-wish)))


;; w
;; An abbreviation for wish.

(define w wish)


;; system-dependent functions: delete-file-if-exists, flush-output, system
;; Uncomment the part for the Scheme system you are using.


;; bigloo part: doesn't work because bigloo can't open a pipe as an input file.
;; builtin functions: system

;;(define delete-file-if-exists (lambda (name)
;;  (delete-file name)))

;;(define flush-output flush-output-port)


;; gambit part
;; builtin functions: flush-output

;;(define delete-file-if-exists (lambda (name)
;;  (system (string-append "/bin/rm -f " name))))

;;(define system ##shell-command)


;; guile part
;; builtin functions: system

(define delete-file-if-exists
  (lambda (name)
    (cond ((file-exists? name)  
	   (delete-file name)))))

;;(define flush-output force-output)


;; SCM part
;; builtin functions: system

;;(define delete-file-if-exists delete-file)

;;(define flush-output force-output)

