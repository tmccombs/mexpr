;; This software is Copyright (c) Thayne McCombs, 2014.
;; Thayne McCombs grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :asdf-user)

(defsystem mexpr-tests
  :description "Tests for mexpr infix syntax library."
  :author "Thayne McCombs <bytecurry.software@gmail.com>"
  :license "LLGPL"
  :depends-on (:mexpr :should-test :named-readtables)
  :perform (test-op (o s)
		    (uiop:symbol-call :should-test '#:test :package :bytecurry.mexpr-tests))
  :serial t
  :components ((:file "test-packages")
	       (:file "mexpr-tests")))
