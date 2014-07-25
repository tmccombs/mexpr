;; This software is Copyright (c) Thayne McCombs, 2014.
;; Thayne McCombs grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :cl-user)

(defpackage :bytecurry.mexpr.tests
  (:nicknames :mexpr-tests)
  (:documentation "Tests for mexpr.")
  (:use :common-lisp
	:bytecurry.mexpr
	:5am)
  (:export :mexpr-test-suite))
