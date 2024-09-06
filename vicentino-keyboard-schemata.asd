(asdf:defsystem "vicentino-keyboard-schemata"
  :depends-on (:cl-svg)
  :serial t
  :components ((:file "package")
               (:file "build-macro")
               (:file "label-definitions")
               (:file "keyboard-definitions")
               (:file "tex-generator")
               (:file "interval-definitions")
               (:file "diagrams")
               (:file "svg-generator")))
