;; This software is Copyright (c) Thayne McCombs, 2014.
;; Thayne McCombs grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :asdf-user)

(defsystem mexpr-tests
  :depends-on (:mexpr :fiveam :named-readtables)
  :perform (test-op (o s)
		    (uiop:symbol-call :fiveam '#:run!
				      (uiop:find-symbol* '#:mexpr-test-suite :bytecurry.mexpr.tests)))
  :serial t
  :components ((:file "test-packages")
	       (:file "mexpr-tests")))
