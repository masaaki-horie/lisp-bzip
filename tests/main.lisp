(defpackage my-compress/tests/main
  (:use :cl
        :my-compress
        :rove))
(in-package :my-compress/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :my-compress)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
