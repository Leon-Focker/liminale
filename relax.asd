(asdf:defsystem #:relax
  :version "0.1"
  :default-component-class "cl-source-file.lsp"
  :description "Generate music for meditation."
  :author "Leon Focker"
  :license "GNU General Public License v3.0"
  :depends-on ()
  :serial t
  :pathname "src/"
  :components ((:file "package")
	       (:file "helpers")
	       (:file "notes")
	       (:file "relax")))

;; EOF relax.asd
