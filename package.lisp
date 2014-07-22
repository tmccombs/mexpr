;; This software is Copyright (c) Thayne McCombs, 2014.
;; Thayne McCombs grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :cl-user)

(defpackage :bytecurry.mexpr
  (:nicknames :mexpr)
  (:documentation "A library for embedding mathematical expressions
in lisp code using traditional infix syntax.")
  (:use :common-lisp 
	:alexandria
	:cl-syntax)
  (:export :infix
	   :defop
	   :infix-reader
	   :enable-infix-syntax
	   :syntax-error
	   :syntax-error-type))
	   
