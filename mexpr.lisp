(in-package :bytecurry.mexpr)

(define-condition syntax-error (error)
  ((type :initarg :type :type 'keyword :reader syntax-error-type))
  (:report (lambda (condition stream)
	     (format stream "Mexpr Syntax Error: ~a." (syntax-error-type condition)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
    (defparameter *operators* (make-hash-table :test 'eq) 
      "Hash map of operator symbols to precedence."))

(defmacro defop (name precedence)
  (declare (symbol name) (integer precedence))
  "Define a new infix operator with the given name and precedence."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *operators*) ,precedence)))

(declaim (inline get-precedence))
(defun get-precedence (op &optional default)
  (declare (symbol op))
  "Get the precedence of an operator."
  (gethash op *operators* default))

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
  (pop (op-state-operands state)))

(defop or 10)
(defop and 10)
(defop = 20)
(defop /= 20)
(defop < 20)
(defop > 20)
(defop >= 20)
(defop <= 20)
(defop + 40)
(defop - 40)
(defop * 50)
(defop / 50)
(defop mod 50)
(defop rem 50)
(defop expt 60)

(defstruct op-state 
  "Structure for state of the operation stacks."
  (operands nil :type list)
  (operators nil :type list))

(defun finalize-operations (state)
  (do () ((null (op-state-operators state)))
    (reduce-state state))
  (when (> (length (op-state-operands state)) 1)
    (error 'syntax-error :type :missing-operator))
  (car (op-state-operands state)))

(defun reduce-state (state)
  (declare (op-state state))
  (let ((op (pop-operator state))
	(right (pop-operand state))
	(left  (pop-operand state)))
    (when (not (and right left)) (error 'syntax-error :type :missing-operand))
    (push-operand (list op left right) state)))

(defun handle-end-group (state)
  (declare (op-state state))
  (loop for next-op = (pop-operator state) until (eq '[ next-op)
       do (let ((right (pop-operand state))
		(left  (pop-operand state)))
	    (when (null next-op) (error 'syntax-error :type :mismatch-group))
	    (when (not (and right left)) (error 'syntax-error :type :missing-operand))
	    (push-operand (list next-op left right) state))))

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

(defmacro mexpr (&rest exprs)
  "Macro to convert an infix expression, into an s-expression."
  (mexpr-impl exprs))



  
      
