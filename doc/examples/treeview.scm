(cond-expand
  (chicken-4 (use pstk))
  (chicken-5 (import pstk)))

;; PS-TK example: display a treeview

(tk-start)
(ttk-map-widgets 'all) ; make sure we are using tile widget set

(tk/wm 'title tk "PS-Tk Example: TreeView")
(tk 'configure 'height: 230 'width: 350)

;; create a tree view within scroll bars
(let ((treeview (tk 'create-widget 'treeview 'columns: '("col1" "col2" "col3")))
      (hsb (tk 'create-widget 'scrollbar 'orient: 'horizontal))
      (vsb (tk 'create-widget 'scrollbar 'orient: 'vertical)))
  ;; associate scrollbars and treeview
  (hsb 'configure 'command: (list treeview 'xview))
  (vsb 'configure 'command: (list treeview 'yview))
  (treeview 'configure 'xscrollcommand: (list hsb 'set))
  (treeview 'configure 'yscrollcommand: (list vsb 'set))
  ;; set up columns in tree view
  (treeview 'column "col1" 'width: 70)
  (treeview 'heading "col1" 'text: "Col 1")
  (treeview 'column "col2" 'width: 70)
  (treeview 'heading "col2" 'text: "Col 2")
  (treeview 'column "col3" 'width: 70)
  (treeview 'heading "col3" 'text: "Col 3")
  ;; insert items into tree view
  (treeview 'insert "" 'end 'id: "item1" 'text: "item 1" 'values: "a b 1")
  (treeview 'insert "" 'end 'id: "subtree1" 'text: "item 2" 'values: "c d 2")
  (treeview 'insert "" 'end 'id: "subtree2" 'text: "item 3" 'values: "e f 3")
  (treeview 'insert "subtree1" 'end 'text: "item 4" 'values: "g h 4")
  (treeview 'insert "subtree1" 'end 'text: "item 5" 'values: "i j 5")
  (treeview 'insert "subtree2" 'end 'text: "item 6" 'values: "k l 6")
  (treeview 'insert "subtree2" 'end 'text: "item 7" 'values: "m n 7")
  (treeview 'insert "subtree2" 'end 'text: "item 8" 'values: "o p 8")
  ;; place tree view and scroll bar
  (tk/grid treeview 'column: 0 'row: 0 'sticky: 'news)
  (tk/grid hsb 'column: 0 'row: 1 'sticky: 'we)
  (tk/grid vsb 'column: 1 'row: 0 'sticky: 'ns)
  ; ensure grid fills the frame
  (tk/grid 'columnconfigure tk 0 'weight: 1)
  (tk/grid 'rowconfigure tk 0 'weight: 1)

  ;; create a label and button to show selection
  (let ((label (tk 'create-widget 'label)))
    (tk/grid label 'column: 0 'row: 2 'pady: 5)
    (tk/grid (tk 'create-widget 'button
                 'text: "Show item"
                 'command: (lambda () (label 'configure 'text: (treeview 'selection))))
             'column: 0 'row: 3 'pady: 5))
  )

(tk-event-loop)
