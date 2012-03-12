;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-64
  (:use)
  (:export
   :test-assert :test-eqv :test-eq :test-equal :test-approximate :test-error
   :test-read-eval-string :test-begin :test-end :test-group
   :test-group-with-cleanup :test-match-name :test-match-nth :test-match-any
   :test-match-all :test-skip :test-expect-fail :test-runner?
   :test-runner-current :test-runner-get :test-runner-simple :test-runner-null
   :test-runner-create :test-runner-factory :test-apply :test-with-runner
   :test-result-kind :test-passed? :test-result-ref :test-result-set!
   :test-result-remove :test-result-clear :test-result-alist
   :test-runner-on-test-begin :test-runner-on-test-begin!
   :test-runner-on-group-begin :test-runner-on-group-begin!
   :test-runner-on-group-end :test-runner-on-group-end! :test-runner-on-bad-count
   :test-runner-on-bad-count! :test-runner-on-bad-end-name
   :test-runner-on-bad-end-name! :test-runner-on-final :test-runner-on-final!
   :test-on-test-begin-simple :test-on-test-end-simple
   :test-on-group-begin-simple :test-on-group-end-simple
   :test-on-bad-count-simple :test-on-bad-end-name-simple :test-on-final-simple
   :test-runner-pass-count :test-runner-fail-count :test-runner-xpass-count
   :test-runner-xfail-count :test-runner-skip-count :test-runner-test-name
   :test-runner-group-path :test-runner-group-stack :test-runner-aux-value
   :test-runner-aux-value! :test-runner-reset))

(defpackage :srfi-64.internal
  (:use :srfi-64 :cl :fiveam :mbe
        :srfi-0 :srfi-9 :srfi-34 :srfi-35 :srfi-39)
  (:shadow :lambda :member :map :assoc :write :loop :do)
  (:shadowing-import-from :srfi-35
                          :condition
                          :make-condition)
  (:shadowing-import-from :srfi-23
                          :error))
