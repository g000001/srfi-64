(cl:in-package :srfi-64.internal)
;; (in-readtable :srfi-64)

(def-suite srfi-64)

(in-suite srfi-64)

(test-runner-reset (test-runner-get))

(test-begin "foo" 0)
(test-end)

(test-begin "foo1" 1)
(test-equal 1 1)
(test-end)

(test-begin "foo2" 100)

(dotimes (i 100)
  (test-equal 1 1))

(test-end)


(test-begin "foo3" 200)

(dotimes (i 200)
  (test-equal 1 1))

(test-end)

(test-result-clear (test-runner-current))

(progn
  (test-begin "foo4" 2)
  (test-equal 1 1)
  ;; (test-equal 1 1)
  (test-error T (eval '(car 8)))
  (test-end))
