;; This software is Copyright (c) Thayne McCombs, 2014.
;; Thayne McCombs grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :bytecurry.mexpr.tests)

(def-suite mexpr-test-suite :description "Tests for mexpr")

(eval-when (:compile-toplevel :load-toplevel :execute)
    (named-readtables:ensure-readtable 'test-readtable
      (named-readtables:copy-named-readtable :standard)))

(named-readtables:in-readtable test-readtable)

(enable-infix-syntax)

(in-suite mexpr-test-suite)

(test operators
  (is (eq t (infix t or nil)))
  (is (eq nil (infix t and nil)))
  (is (= 3 (infix 2 \| 3)))
  (is (= 7 (infix 2 logior 5)))
  (is (= 4 (infix 13 ^ 9)))
  (is (= 99 (infix 45 logxor 78)))
  (is (= 5 (infix 127 & 5)))
  (is (= 8 (infix 127 logand 8)))
  (is (eq t (infix 5 = 5)))
  (is (eq t (infix 5 /= 6)))
  (is (eq t (infix 5 < 6)))
  (is (eq nil (infix 5 > 6)))
  (is (eq t (infix 5 <= 6)))
  (is (eq nil (infix 5 >= 6)))
  (is (= 8 (infix 1 ash 3)))
  (is (= 16 (infix 1 << 4)))
  (is (= 2 (infix 4 >> 1)))
  (is (= 6 (infix 2 + 4)))
  (is (= 4 (infix 10 - 6)))
  (is (= 12 (infix 3 * 4)))
  (is (= 4 (infix 12 / 3)))
  (is (= 2 (infix 12 mod 5)))
  (is (= 2 (infix 12 rem 5)))
  (is (= 2 (infix 12 % 5)))
  (is (= 27 (infix 3 expt 3)))
  (is (= 27 (infix 3 ** 3))))

(test read-macro
  (is (= 7 #n(3 + 4)))
  (is (equal '(+ 3 4) '#n(3 + 4))))

(test precedence
  (is (= 23 (infix 3 + 4 * 5)))
  (is (= 17 (infix 2 ** 2 * 4 + 3 - 2))))
