(in-package :asdf-user)

(defsystem "mexpr"
  :description "mexpr: Macro for infix math expressions."
  :version "0.1.0"
  :author "Thayne McCombs <astrothayne@gmail.com>"
  :license "LLPGL"
  :components((:file "package")
	      (:file "mexpr" :depends-on ("package")))
  :depends-on (:alexandria))
