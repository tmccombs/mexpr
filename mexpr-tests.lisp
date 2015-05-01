;; This software is Copyright (c) Thayne McCombs, 2014.
;; Thayne McCombs grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :bytecurry.mexpr-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
    (named-readtables:ensure-readtable 'test-readtable
      (named-readtables:copy-named-readtable :standard)))

(named-readtables:in-readtable test-readtable)

(enable-infix-syntax)


(deftest operators ()
  (should be eq t (infix t or nil))
  (should be eq nil (infix t and nil))
  (should be = 3 (infix 2 \| 3))
  (should be = 7 (infix 2 logior 5))
  (should be = 4 (infix 13 ^ 9))
  (should be = 99 (infix 45 logxor 78))
  (should be = 5 (infix 127 & 5))
  (should be = 8 (infix 127 logand 8))
  (should be eq t (infix 5 = 5))
  (should be eq t (infix 5 /= 6))
  (should be eq t (infix 5 < 6))
  (should be eq nil (infix 5 > 6))
  (should be eq t (infix 5 <= 6))
  (should be eq nil (infix 5 >= 6))
  (should be = 8 (infix 1 ash 3))
  (should be = 16 (infix 1 << 4))
  (should be = 2 (infix 4 >> 1))
  (should be = 6 (infix 2 + 4))
  (should be = 4 (infix 10 - 6))
  (should be = 12 (infix 3 * 4))
  (should be = 4 (infix 12 / 3))
  (should be = 2 (infix 12 mod 5))
  (should be = 2 (infix 12 rem 5))
  (should be = 2 (infix 12 % 5))
  (should be = 27 (infix 3 expt 3))
  (should be = 27 (infix 3 ** 3)))

(deftest read-macro ()
  (should be = 7 #n(3 + 4))
  (should be equal '(+ 3 4) '#n(3 + 4)))

(deftest precedence ()
  (should be = 23 (infix 3 + 4 * 5))
  (should be = 17 (infix 2 ** 2 * 4 + 3 - 2)))
