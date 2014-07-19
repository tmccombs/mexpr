(in-package :cl-user)

(defpackage :bytecurry.mexpr
  (:nicknames :mexpr)
  (:use :common-lisp :alexandria)
  (:export :infix
	   :defop
	   :syntax-error
	   :syntax-error-type))
	   
