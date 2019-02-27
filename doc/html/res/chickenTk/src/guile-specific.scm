;; guile part

;; ----------------------------------------------------------------
;; from scheme_wish.scm
;;

;; (run-program p)
;; Side effect: runs the program p with input and output bound to streams.

(define run-program
  (lambda (program input-filename output-filename)
    (let ((command #f)
	  (input #f)
	  (output #f)
	  (result #f))
      (delete-file-if-exists input-filename)
      ;;(write "input file deleted")(newline)
      (delete-file-if-exists output-filename)
      ;;(write "output file deleted")(newline)
      (set! result (system (string-append "mknod " input-filename " p")))
      ;;(write result)(newline)
      (set! result (system (string-append "mknod " output-filename " p")))
      ;;(write result)(newline)
      (set! command (string-append program " < " 
				   input-filename " > "
				   output-filename " 2>&1"))
      ;; 2>&1 associates file descriptor 2 (standard error) with the file associated
      ;; with file descriptor 1.
      (set! result (system (string-append "/bin/sh -c \"" command "\" &")))
      ;;(write result)(newline)
      (set! input (open-output-file input-filename))
      ;;(write input)(newline)
      (set! output (open-input-file output-filename))
      ;;(write output)(newline)
      (list input output))))

;; start-wish
;; Starts a wish process.

(define start-wish
  (lambda ()
    (let ((result (run-program *wish-program* *wish-input-filename* *wish-output-filename*)))
      (set! *wish-input* (car result))
      (set! *wish-output* (cadr result)))))

(define *wish-input* #f) ; The input stream of the wish process.

(define *wish-output* #f) ; The output stream of the wish process.

;;
;; builtin functions: system

(define delete-file-if-exists
  (lambda (name)
    (cond ((file-exists? name)  
	   (delete-file name)))))

(define flush-output force-output)

;; ----------------------------------------------------------------
;; from SchemeTk.scm
;;

					;  gensym
					;  string-translate*
					;  error
					;  keyword?
					;  get-keyword
					;  string->keyword
					;  keyword->string
					;  ->string

;; ------------------

;;
;; (gensym) => unique name with prefix "g"
;; (gensym x) => unique name with prefix x
;;
(define gensym
  (let ((count 0))
    (lambda args
      (let ((result (if (null? args) "g" (car args))))
	(set! result (string-append result (number->string count)))
	(set! count (+ count 1))
	(string->symbol result)))))

;;
;; (flush-output)
;; writes waiting chars to output channel
;;
(define flush-output force-output)

;;
;; (starts-with fullstr partstr)
;; returns #t if fullstr starts with partstr
;; used by string-translate*
;;
(define (starts-with fullstr partstr)
  (let ((l (string-length partstr)))
    (and (>= (string-length fullstr) l)
	 (string=? (substring fullstr 0 l) partstr))))

;;
;; (contains-substring fullstr partstr) 
;; returns #t if fullstr contains partstr
;; (contains-substring fullstr partstr index) 
;; returns #t if fullstr contains partstr after index
;; used by string-translate*
;;
(define (contains-substring fullstr partstr . args)
  (let* ((full-len (string-length fullstr))
	 (part-len (string-length partstr))
	 (char1 (car (string->list partstr)))
	 (index (if (null? args) 0 (car args)))
	 (partlist (memq char1 (string->list (substring fullstr index)))))
    (cond
     ((> part-len full-len) #f)
     ((>= index full-len) #f)
     ((not partlist) #f)
     (else
      (let ((try-index (- full-len (length partlist))))
	(if (starts-with (substring fullstr try-index) partstr)
	    try-index
	    (contains-substring fullstr partstr (+ try-index 1))))))))

;;
;; (string-translate-from-to str from to)
;; replaces all occurrences of from in str by to
;; used by string-translate*
;;
(define (string-translate-from-to str from to)
  (let ((index (contains-substring str from)))
    (if index
	(let ((reststr (substring str (+ index (string-length from)))))
	  (string-append
	   (substring str 0 index)
	   to
	   (string-translate-from-to reststr from to)))
	str)))

;; ------------------

;;
;; (string-translate* str '(("from1-1" . "to-1") ("from-2" . "to-2") ... )
;; replaces in str all from-i by to-i.
;;
(define (string-translate* str l)
  (if (null? l)
      str
      (string-translate*
       (string-translate-from-to str (caar l) (cdar l))
       (cdr l))))

;; ==================

;;
;; (get-keyword kwd l)
;; returns value of keyword kwd in list l
;; where l is (kwd1 val1 kwd2 val2 ...)
;;
(define (get-keyword kwd l)
  (let ((l1 (memq kwd l)))
    (and l1 (not (null? (cdr l1))) (cadr l1))))

;;
;; (string->keyword str)
;; converts string to keyword
;;
(define (string->keyword str)
  (symbol->keyword (string->symbol str)))

;;
;; (->string x)
;; converts any x to string
;;
(define (->string x)
  (cond ((unspecified? x) "#<unspecified>")
	((keyword? x)
	 (symbol->string (keyword->symbol x)))
	((symbol? x) (symbol->string x))
	((string? x) x)
	((number? x) (number->string x))
	((procedure? x) "#<procedure>")
	((eq? x #t) "#t")
	((eq? x #f) "#f")
	((vector? x)
	 (string-append "#" (->string (vector->list x))))
	((list? x)
	 (string-append 
	  "("
	  (string-trimleft
	   (apply string-append
		  (map (lambda (y) (string-append " " (->string y))) x)))
	  ")"))
	((pair? x)
	 (do ((x x (cdr x))
	      (cars (list (car x)) (cons (car x) cars)))
	     ((not (pair? x))
	      (string-append 
	       "("
	       (apply string-append
		      (map (lambda (y) (string-append (->string y) " "))
			   (reverse cars)))
	       ". "
	       (->string x)
	       ")"))))
	(else "unknown")))

