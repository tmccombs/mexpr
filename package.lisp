(in-package :cl-user)

(defpackage :bytecurry.mexpr
  (:nicknames :mexpr)
  (:use :common-lisp :alexandria)
  (:export :mexpr
	   :defop
	   :syntax-error
	   :syntax-error-type))
	   
