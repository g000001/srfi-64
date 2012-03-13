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



(progn
  (test-runner-reset (test-runner-current))
  (test-begin "foo")
  (test-equal 1 1)
  (test-equal 1 1)
  (test-error T (eval '(car 8)))
  (test-assert t)
  (test-end))
;>>  %%%% Starting test foo
;>>  # of expected passes      5
;>>
;=>  NIL
