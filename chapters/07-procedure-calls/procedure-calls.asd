(defsystem "procedure-calls"
  :depends-on ("trivia") ;; Add this line to include trivia
  :components
   ((:file "compiler")
    (:file "../../gh-stdlib/defuns")))

