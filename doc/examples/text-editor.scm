(cond-expand
  (chicken-4 (use pstk srfi-13))
  (chicken-5 (import pstk srfi-13 chicken.string)))


(define (get-string-all input-port)
  (let loop ((char-list '()))
    (if (eof-object? (peek-char input-port))
      (reverse-list->string char-list)
      (loop (cons (read-char input-port)
                  char-list)))))

(tk-start)
(ttk-map-widgets 'all) ; make sure we are using tile widget set
(tk/wm 'title tk "Text Editor")
(tk 'configure 'height: 230 'width: 350)

(let ((text (tk 'create-widget 'text)))

  (let ((menubar (tk 'create-widget 'menu))
        (main-menu (tk 'create-widget 'menu))
        (file-menu (tk 'create-widget 'menu)))
    (menubar 'add 'cascade 'menu: main-menu 'label: "Editor" 'underline: 0)
    (menubar 'add 'cascade 'menu: file-menu 'label: "File" 'underline: 0)

    (main-menu 'add 'command 'label: "About" 'underline: 0
               'command: (lambda ()
                           (tk/message-box 'title: "About"
                                           'message: "Simple text editor in Scheme/Tk"
                                           'type: 'ok)))
    (main-menu 'add 'separator)
    (main-menu 'add 'command 'label: "Exit" 'underline: 1
               'command: tk-end)

    (file-menu 'add 'command 'label: "New" 'underline: 0
               'command: (lambda () ; remove all the text
                           (text 'delete 1.0 "end")))
    (file-menu 'add 'command 'label: "Open" 'underline: 0
               'command: (lambda ()
                           (let ((filename (tk/get-open-file)))
                             (unless (string-null? filename)
                               (text 'delete "1.0" "end")
                               (call-with-input-file
                                 filename
                                 (lambda (input-port)
                                   (text 'insert "1.0"
                                         (get-string-all input-port))))))))
    (file-menu 'add 'command 'label: "Save" 'underline: 0
               'command: (lambda ()
                           (let ((filename (tk/get-save-file)))
                             (unless (string-null? filename)
                               (with-output-to-file
                                 filename
                                 (lambda ()
                                   (display (text 'get 1.0 "end"))))))))

    (tk 'configure 'menu: menubar))

  (let ((vscroll (tk 'create-widget 'scrollbar 'orient: 'vertical)))
    (tk/grid text 'column: 0 'row: 0 'sticky: 'news)
    (tk/grid vscroll 'column: 1 'row: 0 'sticky: 'ns)
    ; make text and scrollbar talk to each other
    (vscroll 'configure 'command: (list text 'yview))
    (text 'configure 'yscrollcommand: (list vscroll 'set))
    (tk/grid 'columnconfigure tk 0 'weight: 1)
    (tk/grid 'rowconfigure tk 0 'weight: 1)
    )
  (tk-event-loop))
