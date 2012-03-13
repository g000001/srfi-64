;;;; srfi-64.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-64
  :serial t
  :depends-on (:fiveam
               :srfi-0
               :srfi-5
               :srfi-9
               :srfi-23
               :srfi-34
               :srfi-35
               :srfi-39)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-64")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-64))))
  (load-system :srfi-64)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-64.internal :srfi-64))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
