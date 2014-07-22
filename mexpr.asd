;; This software is Copyright (c) Thayne McCombs, 2014.
;; Thayne McCombs grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :asdf-user)

(defsystem "mexpr"
  :description "mexpr: Macro for infix math expressions."
  :version "0.1.0"
  :author "Thayne McCombs <astrothayne@gmail.com>"
  :license "LLPGL"
  :components((:file "package")
	      (:file "mexpr" :depends-on ("package")))
  :depends-on (:alexandria
	       :cl-syntax))
