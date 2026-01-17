(asdf:defsystem #:liminale
  :version "0.5"
  :default-component-class "cl-source-file.lsp"
  :description "Algorithmic, generative music for meditation and background listening."
  :author "Leon Focker"
  :license "GNU General Public License v3.0"
  :depends-on ()
  :serial t
  :pathname "src/"
  :components ((:file "package")
	       (:file "helpers")
	       (:file "modulators")
	       (:file "notes")
	       (:file "synths")
	       ;;(:file "main")
	       ))

;; EOF liminale.asd
