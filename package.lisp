(in-package :cl-user)

(defpackage :bytecurry.mexpr
  (:nicknames :mexpr)
  (:use :common-lisp 
	:alexandria
	:cl-syntax)
  (:export :infix
	   :defop
	   :infix-reader
	   :enable-infix-syntax
	   :syntax-error
	   :syntax-error-type))
	   
