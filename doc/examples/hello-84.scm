;; example to show use of a different tclsh

(cond-expand
  (chicken-4 (use pstk))
  (chicken-5 (import pstk)))

(tk-start "tclsh8.4")

(tk/pack
  (tk 'create-widget 'button 'text: "Hello"
      'command: (lambda () (display "Hello world") (newline)))
  'padx: 20 'pady: 20)
(tk-event-loop)
