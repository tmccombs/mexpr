;; This software is Copyright (c) Thayne McCombs, 2014.
;; Thayne McCombs grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :asdf-user)

(defsystem mexpr
  :description "Macro for infix math expressions."
  :long-description "mexpr is a library which contains a macro for
embedding mathematical expressions in lisp code with a more traditional
infix syntax. It also contains a reader macro for said syntax, and a macro
to extend the syntax with additional operators."
  :version "0.2.0"
  :author "Thayne McCombs <bytecurry.software@gmail.com>"
  :license "LLPGL"
  :serial t
  :components ((:file "package")
               (:file "mexpr")
               (:file "operators"))
  :depends-on (:alexandria
               :cl-syntax)
  :in-order-to ((test-op (test-op :mexpr-tests))))
