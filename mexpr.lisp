;; This software is Copyright (c) Thayne McCombs, 2014.
;; Thayne McCombs grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :bytecurry.mexpr)

(deftype form-head ()
  "The first subform of a compound form."
  '(or symbol (cons (eql lambda) cons)))

(define-condition syntax-error (error)
  ((type :initarg :type :type 'keyword :reader syntax-error-type))
  (:report (lambda (condition stream)
             (format stream "Mexpr Syntax Error: ~a." (syntax-error-type condition))))
  (:documentation "Error due to invalid syntax in infix expression"))


(defstruct op-state
  "Structure for state of the operation stacks."
  (operands nil :type list)
  (operators nil :type list))

(eval-when (:compile-toplevel :load-toplevel :execute)
    (defstruct operator
      "Struct for information about an operator."
      (precedence 0 :type integer :read-only t)
      (func 'values :type form-head))

    (defparameter *operators* (make-hash-table :test 'eq)
      "Hash map of operator symbols to precedence."))

(defmacro defop (name precedence &optional (func name))
  (declare (symbol name) (type (integer 1 *) precedence) (form-head func))
  "Define a new infix operator with the given name and precedence.

NAME is the symbol which will be used as a binary operator in an infix expression
PRECEDENCE is a positive integer for the precedence of the operator
FUNC is a symbol or lambda form that the operator will translate into during macro-expansion"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *operators*) (make-operator :precedence ,precedence :func ',func))
     (values)))

(defun get-precedence (op &optional default)
  (declare (symbol op))
  "Get the precedence of an operator."
  (if-let ((operator (gethash op *operators*)))
    (operator-precedence operator)
    default))

(defun get-operator-func (op)
  (declare (symbol op))
  "Get the symbol or lambda form for the operator."
  (operator-func (gethash op *operators*)))

(declaim (inline push-operator))
(defun push-operator (op state)
  (push op (op-state-operators state)))

(declaim (inline push-operand))
(defun push-operand (op state)
  (push op (op-state-operands state)))

(declaim (inline pop-operator))
(defun pop-operator (state)
  (pop (op-state-operators state)))

(declaim (inline pop-operand))
(defun pop-operand (state)
  (when (null (op-state-operands state))
    (error 'syntax-error :type :missing-operand))
  (pop (op-state-operands state)))


(defun finalize-operations (state)
  (do () ((null (op-state-operators state)))
    (reduce-state state))
  (when (> (length (op-state-operands state)) 1)
    (error 'syntax-error :type :missing-operator))
  (car (op-state-operands state)))

(defun do-operation (operator state)
  (declare (symbol operator) (op-state state))
  "Do the operation for the operands on the stack."
  (let ((right (pop-operand state))
	(left  (pop-operand state)))
    (push-operand (list (get-operator-func operator) left right) state)))


(defun reduce-state (state)
  (declare (op-state state))
  (let ((op (pop-operator state)))
    (do-operation op state)))

(defun handle-end-group (state)
  (declare (op-state state))
  (loop for next-op = (pop-operator state) until (eq '[ next-op)
       do (progn
	    (when (not next-op) (error 'syntax-error :type :mismatch-group))
	    (do-operation next-op state))))

(defun handle-expr (state expr)
  (declare (op-state state))
  (if (symbolp expr)
      (cond ((string= "[" (symbol-name expr)) (push-operator '[ state))
	    ((string= "]" (symbol-name expr)) (handle-end-group state))
	    (t (if-let ((prec (get-precedence expr)))
			 (let ((last-op (first (op-state-operators state))))
			   (when (>= (get-precedence last-op 0) prec)
			     (reduce-state state))
			   (push-operator expr state))
			 (push-operand expr state))))
      (push-operand expr state)))

(defun mexpr-impl (exprs)
  "Convert an infix expression, into an s-expression --implementation."
  (let ((state (make-op-state)))
    (dolist (expr exprs)
      (handle-expr state expr))
    (finalize-operations state)))

(defmacro infix (&rest exprs)
  "Macro to convert an infix expression, into an s-expression."
  (mexpr-impl exprs))

(defun infix-reader (stream char narg)
  (declare (ignore char narg) (stream stream))
  "Reader macro function to read infix expressions."
  (mexpr-impl (read stream t nil t)))

(define-package-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\n #'infix-reader))

(defmacro enable-infix-syntax ()
  "Enable infix syntax with '#n', for example:
#n(3 + 4) => 7"
  '(use-syntax :mexpr))
