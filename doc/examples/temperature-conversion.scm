(cond-expand
  (chicken-4 (use pstk))
  (chicken-5 (import pstk)))

(define (celsius->fahrenheit item)
  (let ((number (string->number item)))
    (if (number? number)
      (+ (* number 9/5) 32)
      0.0)))

(tk-start)
(tk/wm 'title tk "Celsius to Fahrenheit")

(let* ((celsius (tk 'create-widget 'entry))
       (label (tk 'create-widget 'label))
       (button (tk 'create-widget 'button
		   'text: 'Calculate
		   'command: (lambda ()
			       (label 'configure
				      'text: (number->string (celsius->fahrenheit (celsius 'get))))))))
  ; layout widgets in a grid
  (tk/grid celsius 'column: 2 'row: 1 'sticky: 'we 'padx: 5 'pady: 5)
  (tk/grid label 'column: 2 'row: 2 'sticky: 'we 'padx: 5 'pady: 5)
  (tk/grid button 'column: 2 'row: 3 'sticky: 'we 'padx: 5 'pady: 5)
  (tk/grid (tk 'create-widget 'label 'text: "celsius")
	   'column: 3 'row: 1 'sticky: 'w 'padx: 5 'pady: 5)
  (tk/grid (tk 'create-widget 'label 'text: "is")
	   'column: 1 'row: 2 'sticky: 'e 'padx: 5 'pady: 5)
  (tk/grid (tk 'create-widget 'label 'text: "fahrenheit")
	   'column: 3 'row: 2 'sticky: 'w 'padx: 5 'pady: 5)

  (tk-event-loop))
