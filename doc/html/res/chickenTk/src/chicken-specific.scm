(use posix)

;;
;; (run-program p)
;; Side effect: runs the program p with input and output bound to streams.
;;
(define (run-program program . args)
  (let-values
   (((in out pid) (process (string-append program " 2>&1"))))
   (list out in)))

;;
;; start-wish
;; Starts a wish process.
;;
(define start-wish
  (lambda ()
    (let ((result (run-program *wish-program*)))
      (set! *wish-input* (car result))
      (set! *wish-output* (cadr result)))))

;;
;; (catch thunk)
;; returns (success result)
;; or (error "reason of error\n")
;;
(define catch
  (lambda (thunk)
    (condition-case
     (list 'success (thunk))
     (err ()
	  (list 'error
		(with-output-to-string
		  (lambda ()
		    (print-error-message
		     err
		     (current-output-port)))))))))

;;
;; (text-in-last-line-of w)
;; returns char sequence of last text line of text-widget w
;;
(define (eval-last-line-of w)
  (string-append 
   (with-output-to-string
     (lambda ()
       (write
	(eval
	 (with-input-from-string
	     (w 'get '(end - 1 lines) '(end - 1 chars))
	   read)))))
   nl))

;;
;; (tk/console)
;; command-line interface
;;
(define (tk/console)
  (let* ((toplevel (tk 'create-widget 'toplevel))
	 (output (toplevel 'create-widget 'scrolledtext)))
    (tk/wm 'title toplevel 'Console)
    (tk/pack output
	     #:expand 'yes
	     #:fill 'both)
    (output 'insert 'end
	    (string-append
	     "Chicken/Tk is brought to you by Wolf-Dieter Busch." nl
	     "This is a console for your 1st exercises (and for mine)." nl))
    (tk/bind output '<Return>
	     (lambda ()
	       (cond ((string=? 
		       (output 'compare 'insert '< '(end - 1 lines)) "0")
		      (let ((result 
			     (catch (lambda () (eval-last-line-of output)))))
			(output 'insert 'end nl)
			(output 'insert 'end (cadr result))
			(output 'mark 'set 'insert 'end)
			(output 'see 'insert)))
		     (else 
		      (output 'delete '(end - 1 lines) '(end - 1 chars))
		      (output 'insert 'end
			      (output 'get 
				      '(insert linestart) 
				      '(insert lineend)))
		      (output 'mark 'set 'insert 'end)
		      (output 'see 'end)))))
    (tk/bind output '<Return> '+break)))


