(asdf:defsystem "keyboard-generator"
  :depends-on (:cl-svg)
  :serial t
  :components ((:file "keyboard-generator")
               (:file "build-macro")
               (:file "label-definitions")
               (:file "keyboard-definitions")
               (:file "tex-generator")
               (:file "interval-definitions")
               (:file "svg-generator")))
