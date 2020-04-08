;;;; srfi-64.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)


(defsystem :srfi-64
  :version "20200409"
  :description "SRFI 64 for CL: A Scheme API for test suites"
  :long-description "SRFI 64 for CL: A Scheme API for test suites
https://srfi.schemers.org/srfi-64"
  :author "Per Bothner"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:srfi-0
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


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-64))))
  (let ((name "https://github.com/g000001/srfi-64")
        (nickname :srfi-64))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


;;; *EOF*
