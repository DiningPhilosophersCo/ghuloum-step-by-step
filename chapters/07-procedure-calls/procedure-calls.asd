(defsystem "procedure-calls"
  :depends-on ("trivia") ;; Add this line to include trivia
  :components
  ((:file "../../gh-stdlib/defuns")
   (:file "compiler")))

