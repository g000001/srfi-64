;;;; srfi-64.lisp

(cl:in-package :srfi-64.internal)

;; Copyright (c) 2005, 2006 Per Bothner
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

#|(cond-expand
 (chicken
  (require-extension syntax-case))
 (guile
  (use-modules (ice-9 syncase) (srfi srfi-9)
	       ;;(srfi srfi-34) (srfi srfi-35) - not in Guile 1.6.7
	       (srfi srfi-39)))
 (sisc
  (require-extension (srfi 9 34 35 39)))
 (kawa
  (module-compile-options warn-undefined-variable: 'T
			  warn-invoke-unknown-method: 'T)
  (provide 'srfi-64)
  (provide 'testing)
  (require 'srfi-34)
  (require 'srfi-35))
 (:else ()
  ))|#

#|(cond-expand
 (kawa
  (define-syntax %test-export
    (syntax-rules ()
      ((%test-export test-begin . other-names)
       (module-export %test-begin . other-names)))))
 (:else
 (define-syntax %test-export
    (syntax-rules ()
      ((%test-export . names) (if 'NIL 'NIL))))))|#

(define-syntax %test-export
  (syntax-rules ()
    ((%test-export . names) :undef) ))

;; List of exported names
#|(%test-export
 test-begin ;; must be listed first, since in Kawa (at least) it is "magic".
 test-end test-assert test-eqv test-eq test-equal
 test-approximate test-assert test-error test-apply test-with-runner
 test-match-nth test-match-all test-match-any test-match-name
 test-skip test-expect-fail test-read-eval-string
 test-runner-group-path test-group-with-cleanup
 test-result-ref test-result-set! test-result-clear test-result-remove
 test-result-kind test-passed?
 test-log-to-file
 ; Misc test-runner functions
 test-runner? test-runner-reset test-runner-null
 test-runner-simple test-runner-current test-runner-factory test-runner-get
 test-runner-create test-runner-test-name
 ;; test-runner field setter and getter functions - see %test-record-define:
 test-runner-pass-count test-runner-pass-count!
 test-runner-fail-count test-runner-fail-count!
 test-runner-xpass-count test-runner-xpass-count!
 test-runner-xfail-count test-runner-xfail-count!
 test-runner-skip-count test-runner-skip-count!
 test-runner-group-stack test-runner-group-stack!
 test-runner-on-test-begin test-runner-on-test-begin!
 test-runner-on-test-end test-runner-on-test-end!
 test-runner-on-group-begin test-runner-on-group-begin!
 test-runner-on-group-end test-runner-on-group-end!
 test-runner-on-final test-runner-on-final!
 test-runner-on-bad-count test-runner-on-bad-count!
 test-runner-on-bad-end-name test-runner-on-bad-end-name!
 test-result-alist test-result-alist!
 test-runner-aux-value test-runner-aux-value!
 ;; default/simple call-back functions, used in default test-runner,
 ;; but can be called to construct more complex ones.
 test-on-group-begin-simple test-on-group-end-simple
 test-on-bad-count-simple test-on-bad-end-name-simple
 test-on-final-simple test-on-test-end-simple
 test-on-final-simple)|#

#|(cond-expand
 (srfi-9
  (define-syntax %test-record-define
    (syntax-rules ()
      ((%test-record-define alloc runner? (name index setter getter) ***)
       (define-record-type test-runner
	 (alloc)
	 runner?
	 (name setter getter) ***)))))
 (:else
  (define-function %test-runner-cookie (list "test-runner"))
  (define-syntax %test-record-define
    (syntax-rules ()
      ((%test-record-define alloc runner? (name index getter setter) ***)
       (begin
	 (define-function (runner? obj)
	   (and (vector? obj)
		(> (vector-length obj) 1)
		(eq (vector-ref obj 0) %test-runner-cookie)))
	 (define-function (alloc)
	   (let ((runner (make-vector 22)))
	     (vector-set! runner 0 %test-runner-cookie)
	     runner))
	 (begin
	   (define-function (getter runner)
	     (vector-ref runner index)) ***)
	 (begin
	   (define-function (setter runner value)
	     (vector-set! runner index value)) ***)))))))|#

(define-syntax %test-record-define
  (syntax-rules ()
    ((%test-record-define alloc runner? (name index setter getter) ***)
     (define-record-type test-runner
       (alloc)
       runner?
       (name setter getter) ***))))

(%test-record-define
 %test-runner-alloc test-runner?
 ;; Cumulate count of all tests that have passed and were expected to.
 (pass-count 1 test-runner-pass-count test-runner-pass-count!)
 (fail-count 2 test-runner-fail-count test-runner-fail-count!)
 (xpass-count 3 test-runner-xpass-count test-runner-xpass-count!)
 (xfail-count 4 test-runner-xfail-count test-runner-xfail-count!)
 (skip-count 5 test-runner-skip-count test-runner-skip-count!)
 (skip-list 6 %test-runner-skip-list %test-runner-skip-list!)
 (fail-list 7 %test-runner-fail-list %test-runner-fail-list!)
 ;; Normally 'T, except when in a test-apply.
 (run-list 8 %test-runner-run-list %test-runner-run-list!)
 (skip-save 9 %test-runner-skip-save %test-runner-skip-save!)
 (fail-save 10 %test-runner-fail-save %test-runner-fail-save!)
 (group-stack 11 test-runner-group-stack test-runner-group-stack!)
 (on-test-begin 12 test-runner-on-test-begin test-runner-on-test-begin!)
 (on-test-end 13 test-runner-on-test-end test-runner-on-test-end!)
 ;; Call-back when entering a group. Takes (runner suite-name count).
 (on-group-begin 14 test-runner-on-group-begin test-runner-on-group-begin!)
 ;; Call-back when leaving a group.
 (on-group-end 15 test-runner-on-group-end test-runner-on-group-end!)
 ;; Call-back when leaving the outermost group.
 (on-final 16 test-runner-on-final test-runner-on-final!)
 ;; Call-back when expected number of tests was wrong.
 (on-bad-count 17 test-runner-on-bad-count test-runner-on-bad-count!)
 ;; Call-back when name in test=end doesn't match test-begin.
 (on-bad-end-name 18 test-runner-on-bad-end-name test-runner-on-bad-end-name!)
 ;; Cumulate count of all tests that have been done.
 (total-count 19 %test-runner-total-count %test-runner-total-count!)
 ;; Stack (list) of (count-at-start . expected-count):
 (count-list 20 %test-runner-count-list %test-runner-count-list!)
 (result-alist 21 test-result-alist test-result-alist!)
 ;; Field can be used by test-runner for any purpose.
 ;; test-runner-simple uses it for a log file.
 (aux-value 22 test-runner-aux-value test-runner-aux-value!)
)

(define-function (test-runner-reset runner)
  (test-runner-pass-count! runner 0)
  (test-runner-fail-count! runner 0)
  (test-runner-xpass-count! runner 0)
  (test-runner-xfail-count! runner 0)
  (test-runner-skip-count! runner 0)
  (%test-runner-total-count! runner 0)
  (%test-runner-count-list! runner '())
  (%test-runner-run-list! runner 'T)
  (%test-runner-skip-list! runner '())
  (%test-runner-fail-list! runner '())
  (%test-runner-skip-save! runner '())
  (%test-runner-fail-save! runner '())
  (test-runner-group-stack! runner '()) )

(define-function (test-runner-group-path runner)
  (reverse (test-runner-group-stack runner)))

(define-function (%test-null-callback runner)
  (declare (ignore runner))
  'NIL)

(define-function (test-runner-null)
  (let ((runner (%test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner (constantly nil))
    (test-runner-on-group-end! runner #'%test-null-callback)
    (test-runner-on-final! runner #'%test-null-callback)
    (test-runner-on-test-begin! runner #'%test-null-callback)
    (test-runner-on-test-end! runner #'%test-null-callback)
    (test-runner-on-bad-count! runner (constantly nil))
    (test-runner-on-bad-end-name! runner (constantly nil))
    runner ))

;; Not part of the specification.  FIXME
;; Controls whether a log file is generated.
(defvar test-log-to-file 'T)

(define-function (test-runner-simple)
  (let ((runner (%test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner #'test-on-group-begin-simple)
    (test-runner-on-group-end! runner #'test-on-group-end-simple)
    (test-runner-on-final! runner #'test-on-final-simple)
    (test-runner-on-test-begin! runner #'test-on-test-begin-simple)
    (test-runner-on-test-end! runner #'test-on-test-end-simple)
    (test-runner-on-bad-count! runner #'test-on-bad-count-simple)
    (test-runner-on-bad-end-name! runner #'test-on-bad-end-name-simple)
    runner ))

(define-function test-runner-current (make-parameter 'NIL))
(define-function test-runner-factory (make-parameter #'test-runner-simple))

#|(cond-expand
 (srfi-39)

 (:else
  (define-function %test-runner-current 'NIL)
  (define-syntax test-runner-current
    (syntax-rules ()
      ((test-runner-current)
       %test-runner-current)
      ((test-runner-current runner)
       (set! %test-runner-current runner))))
  (define-function %test-runner-factory test-runner-simple)
  (define-syntax test-runner-factory
    (syntax-rules ()
      ((test-runner-factory)
       %test-runner-factory)
      ((test-runner-factory runner)
       (set! %test-runner-factory runner))))))|#

;; A safer wrapper to test-runner-current.
(define-function (test-runner-get)
  (let ((r (test-runner-current)))
    (if (not r)
	(error "test-runner not initialized - test-begin missing?"))
    r))

(define-function (%test-specificier-matches spec runner)
  (funcall spec runner))

(define-function (test-runner-create)
  (funcall (test-runner-factory)))

(define-function (%test-any-specifier-matches list runner)
  (let ((result 'NIL))
    (srfi-5:let loop ((l list))
      (cond ((null? l) result)
	    (:else
	     (if (%test-specificier-matches (car l) runner)
		 (set! result 'T))
	     (loop (cdr l)))))))

;; Returns 'NIL, 'T, or 'xfail.
(define-function (%test-should-execute runner)
  (let ((run (%test-runner-run-list runner)))
    (cond ((or
	    (not (or (eqv? run 'T)
		     (%test-any-specifier-matches run runner) ))
	    (%test-any-specifier-matches
	     (%test-runner-skip-list runner)
	     runner ))
           (test-result-set! runner 'result-kind 'skip)
           'NIL )
	  ((%test-any-specifier-matches
	    (%test-runner-fail-list runner)
	    runner )
	   (test-result-set! runner 'result-kind 'xfail)
	   'xfail )
	  (:else 'T) )))

(define-function (%test-begin suite-name count)
  (if (not (test-runner-current))
      (test-runner-current (test-runner-create)))
  (let ((runner (test-runner-current)))
    (funcall (test-runner-on-group-begin runner) runner suite-name count)
    (%test-runner-skip-save! runner
			       (cons (%test-runner-skip-list runner)
				     (%test-runner-skip-save runner)))
    (%test-runner-fail-save! runner
			       (cons (%test-runner-fail-list runner)
				     (%test-runner-fail-save runner)))
    (%test-runner-count-list! runner
			     (cons (cons (%test-runner-total-count runner)
					 count)
				   (%test-runner-count-list runner)))
    (test-runner-group-stack! runner (cons suite-name
					(test-runner-group-stack runner)))))
#|(cond-expand
 (kawa
  ;; Kawa has test-begin built in, implemented as:
  ;; (begin
  ;;   (cond-expand (srfi-64 #!void) (:else (require 'srfi-64)))
  ;;   (%test-begin suite-name [count]))
  ;; This puts test-begin but only test-begin in the default environment.,
  ;; which makes normal test suites loadable without non-portable commands.
  )
 (:else
  ))|#

(define-syntax test-begin
  (syntax-rules ()
    ((test-begin suite-name)
     (%test-begin suite-name 'NIL) )
    ((test-begin suite-name count)
     (%test-begin suite-name count) )))

(define-function (test-on-group-begin-simple runner suite-name count)
  (declare (ignore count))
  (if (null? (test-runner-group-stack runner))
      (begin
	(display "%%%% Starting test ")
	(display suite-name)
	(if test-log-to-file
	    (let* ((log-file-name
		    (if (string? test-log-to-file) test-log-to-file
			(string-append suite-name ".log") ))
		   (log-file
		    (open log-file-name
                          :direction :output
                          :if-exists :supersede) )
                   (*standard-output* log-file))
	      (display "%%%% Starting test " log-file)
	      (display suite-name log-file)
	      (newline log-file)
	      (test-runner-aux-value! runner log-file)
	      (display "  (Writing full log to \"")
	      (display log-file-name)
	      (display "\")")
              (display "end" log-file)
              (close log-file)))
	(newline) ))
  (let ((log (test-runner-aux-value runner)))
    (if (output-stream-p log)
	(begin
	  (display "Group begin: " log)
	  (display suite-name log)
	  (newline log) )))
  'NIL )

(define-function (test-on-group-end-simple runner)
  (let ((log (test-runner-aux-value runner)))
    (if (output-stream-p log)
	(begin
	  (display "Group end: " log)
	  (display (car (test-runner-group-stack runner)) log)
	  (newline log) )))
  'NIL )

(define-function (%test-on-bad-count-write runner count expected-count port)
  (declare (ignore runner))
  (display "*** Total number of tests was " port)
  (display count port)
  (display " but should be " port)
  (display expected-count port)
  (display ". ***" port)
  (newline port)
  (display "*** Discrepancy indicates testsuite error or exceptions. ***" port)
  (newline port))

(define-function (test-on-bad-count-simple runner count expected-count)
  (%test-on-bad-count-write runner count expected-count *standard-output*)
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(%test-on-bad-count-write runner count expected-count log))))

(define-function (test-on-bad-end-name-simple runner begin-name end-name)
  (let ((msg (string-append (%test-format-line runner) "test-end " begin-name
			    " does not match test-begin " end-name)))
    (error msg)))


(define-function (%test-final-report1 value label port)
  (if (> value 0)
      (begin
	(display label port)
	(display value port)
	(newline port) )))

(define-function (%test-final-report-simple runner port)
  (%test-final-report1 (test-runner-pass-count runner)
		      "# of expected passes      " port)
  (%test-final-report1 (test-runner-xfail-count runner)
		      "# of expected failures    " port)
  (%test-final-report1 (test-runner-xpass-count runner)
		      "# of unexpected successes " port)
  (%test-final-report1 (test-runner-fail-count runner)
		      "# of unexpected failures  " port)
  (%test-final-report1 (test-runner-skip-count runner)
		      "# of skipped tests        " port))

(define-function (test-on-final-simple runner)
  (%test-final-report-simple runner *standard-output*)
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(%test-final-report-simple runner log) )))

(define-function (%test-format-line runner)
  (let* ((line-info (test-result-alist runner))
         (source-file (assq 'source-file line-info))
         (source-line (assq 'source-line line-info))
         (file (if source-file (cdr source-file) "")) )
    (if source-line
        (string-append file ":"
                       (princ-to-string (cdr source-line)) ": ")
        "" )))

(define-function (%test-end suite-name line-info)
  (let* ((r (test-runner-get))
	 (groups (test-runner-group-stack r))
	 (line (%test-format-line r)) )
    (test-result-alist! r line-info)
    (if (null? groups)
	(let ((msg (string-append line "test-end not in a group")))
	  (error msg) ))
    (if (and suite-name (not (equal? suite-name (car groups))))
	(funcall (test-runner-on-bad-end-name r) r suite-name (car groups)) )
    (let* ((count-list (%test-runner-count-list r))
	   (expected-count (cdar count-list))
	   (saved-count (caar count-list))
	   (group-count (- (%test-runner-total-count r) saved-count)) )
      (if (and expected-count
	       (not (= expected-count group-count)) )
	  (funcall (test-runner-on-bad-count r) r group-count expected-count) )
      (funcall (test-runner-on-group-end r) r)
      (test-runner-group-stack! r (cdr (test-runner-group-stack r)))
      (%test-runner-skip-list! r (car (%test-runner-skip-save r)))
      (%test-runner-skip-save! r (cdr (%test-runner-skip-save r)))
      (%test-runner-fail-list! r (car (%test-runner-fail-save r)))
      (%test-runner-fail-save! r (cdr (%test-runner-fail-save r)))
      (%test-runner-count-list! r (cdr count-list))
      (if (null? (test-runner-group-stack r))
	  (funcall (test-runner-on-final r) r) ))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group suite-name . body)
     (let ((r (test-runner-current)))
       ;; Ideally should also set line-number, if available.
       (test-result-alist! r (list (cons 'test-name suite-name)))
       (if (%test-should-execute r)
	   (dynamic-wind
	       (lambda () (test-begin suite-name))
	       (lambda () . body)
	       (lambda () (test-end  suite-name))))))))

(define-syntax test-group-with-cleanup
  (syntax-rules ()
    ((test-group-with-cleanup suite-name form cleanup-form)
     (test-group suite-name
                 (dynamic-wind
                  (lambda () 'NIL)
                  (lambda () form)
                  (lambda () cleanup-form) )))
    ((test-group-with-cleanup suite-name cleanup-form)
     (test-group-with-cleanup suite-name 'NIL cleanup-form) )
    ((test-group-with-cleanup suite-name form1 form2 form3 . rest)
     (test-group-with-cleanup suite-name (begin form1 form2) form3 . rest) )))

(define-function (test-on-test-begin-simple runner)
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(let* ((results (test-result-alist runner))
	       (source-file (assq 'source-file results))
	       (source-line (assq 'source-line results))
	       (source-form (assq 'source-form results))
	       (test-name (assq 'test-name results)) )
	  (display "Test begin:" log)
	  (newline log)
	  (if test-name (%test-write-result1 test-name log))
	  (if source-file (%test-write-result1 source-file log))
	  (if source-line (%test-write-result1 source-line log))
	  (if source-file (%test-write-result1 source-form log)) ))))

(define-syntax test-result-ref
  (syntax-rules ()
    ((test-result-ref runner pname)
     (test-result-ref runner pname 'NIL))
    ((test-result-ref runner pname default)
     (let ((p (assq pname (test-result-alist runner))))
       (if p (cdr p) default)))))

(define-function (test-on-test-end-simple runner)
  (let ((log (test-runner-aux-value runner))
	(kind (test-result-ref runner 'result-kind)) )
    (if (memq kind '(fail xpass))
	(let* ((results (test-result-alist runner))
	       (source-file (assq 'source-file results))
	       (source-line (assq 'source-line results))
	       (test-name (assq 'test-name results)) )
	  (if (or source-file source-line)
	      (begin
		(if source-file (display (cdr source-file)))
		(display ":")
		(if source-line (display (cdr source-line)))
		(display ": ") ))
	  (display (if (eq? kind 'xpass) "XPASS" "FAIL"))
	  (if test-name
	      (begin
		(display " ")
		(display (cdr test-name)) ))
	  (newline) ))
    (if (output-port? log)
	(begin
	  (display "Test end:" log)
	  (newline log)
	  (srfi-5:let loop ((list (test-result-alist runner)))
               (if (pair? list)
                   (let ((pair (car list)))
                     ;; Write out properties not written out by on-test-begin.
                     (if (not (memq (car pair)
                                    '(test-name source-file source-line source-form) ))
                         (%test-write-result1 pair log) )
                     (loop (cdr list)) )))))))

(define-function (%test-write-result1 pair port)
  (display "  " port)
  (display (car pair) port)
  (display ": " port)
  (cl:write (cdr pair) :stream port)
  (newline port))

(define-function (test-result-set! runner pname value)
  (let* ((alist (test-result-alist runner))
	 (p (assq pname alist)))
    (if p
	(set-cdr! p value)
	(test-result-alist! runner (cons (cons pname value) alist)))))

(define-function (test-result-clear runner)
  (test-result-alist! runner '()))

(define-function (test-result-remove runner pname)
  (let* ((alist (test-result-alist runner))
	 (p (assq pname alist)) )
    (if p
	(test-result-alist! runner
                            (srfi-5:let loop ((r alist))
                                        (if (eq? r p) (cdr r)
                                            (cons (car r) (loop (cdr r))) ))))))

(define-function (test-result-kind . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-current))))
    (test-result-ref runner 'result-kind)))

(define-function (test-passed? . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-get))))
    (memq (test-result-ref runner 'result-kind) '(pass xpass))))

(define-function (%test-report-result)
  (let* ((r (test-runner-get))
	 (result-kind (test-result-kind r)))
    (case result-kind
      ((pass)
       (test-runner-pass-count! r (+ 1 (test-runner-pass-count r))))
      ((fail)
       (test-runner-fail-count!	r (+ 1 (test-runner-fail-count r))))
      ((xpass)
       (test-runner-xpass-count! r (+ 1 (test-runner-xpass-count r))))
      ((xfail)
       (test-runner-xfail-count! r (+ 1 (test-runner-xfail-count r))))
      (otherwise
       (test-runner-skip-count! r (+ 1 (test-runner-skip-count r)))))
    (%test-runner-total-count! r (+ 1 (%test-runner-total-count r)))
    (funcall (test-runner-on-test-end r) r)))

#|(cond-expand
 (guile
  (define-syntax %test-evaluate-with-catch
    (syntax-rules ()
      ((%test-evaluate-with-catch test-expression)
       (catch 'T (lambda () test-expression) (lambda (key . args) 'NIL))))))
 (kawa
  (define-syntax %test-evaluate-with-catch
    (syntax-rules ()
      ((%test-evaluate-with-catch test-expression)
       (try-catch test-expression
		  (ex <java.lang.Throwable>
		      (test-result-set! (test-runner-current) 'actual-error ex)
		      'NIL))))))
 (srfi-34
  )
 (chicken
  (define-syntax %test-evaluate-with-catch
    (syntax-rules ()
      ((%test-evaluate-with-catch test-expression)
       (condition-case test-expression (ex () 'NIL))))))
 (:else
  (define-syntax %test-evaluate-with-catch
    (syntax-rules ()
      ((%test-evaluate-with-catch test-expression)
       test-expression)))))|#

;;; ???
(define-syntax %test-evaluate-with-catch
  (syntax-rules ()
    ((%test-evaluate-with-catch test-expression)
     (guard (err
             (:else 'NIL))
            test-expression))))

#|(guard (err
        ((typep err 'cl:condition) :cl-error)
        (:else 'NIL))
       #|(error "")|#
       (raise 'foo)
       )|#

#|(cond-expand
 ((or kawa mzscheme)
  (cond-expand
   (mzscheme
    (define-for-syntax (%test-syntax-file form)
      (let ((source (syntax-source form)))
	(cond ((string? source) file)
				((path? source) (path->string source))
				(:else 'NIL)))))
   (kawa
    (define-function (%test-syntax-file form)
      (syntax-source form))))
  (define-for-syntax (%test-source-line2 form)
    (let* ((line (syntax-line form))
	   (file (%test-syntax-file form))
	   (line-pair (if line (list (cons 'source-line line)) '())))
      (cons (cons 'source-form (syntax-object->datum form))
	    (if file (cons (cons 'source-file file) line-pair) line-pair)))))
 (:else
  ))|#

(define-function (%test-source-line2 form)
  (declare (ignore form))
  '())

(define-function (%test-on-test-begin r)
  (%test-should-execute r)
  (funcall (test-runner-on-test-begin r) r)
  (not (eq? 'skip (test-result-ref r 'result-kind))))

(define-function (%test-on-test-end r result)
  (test-result-set! r 'result-kind
                    (if (eq? (test-result-ref r 'result-kind) 'xfail)
                        (if result 'xpass 'xfail)
                        (if result 'pass 'fail) )))

(define-function (test-runner-test-name runner)
  (test-result-ref runner 'test-name ""))

(define-syntax %test-comp2body
  (syntax-rules ()
    ((%test-comp2body r comp expected expr)
     (progn
       (if (%test-on-test-begin r)
           (let ((exp expected))
             (test-result-set! r 'expected-value exp)
             (let ((res (%test-evaluate-with-catch expr)))
               (test-result-set! r 'actual-value res)
               (%test-on-test-end r (comp exp res)) )))
       (%test-report-result) ))))

(define-function (%test-approximimate= error)
  (lambda (value expected)
    (and (>= value (- expected error))
         (<= value (+ expected error)))))

(define-syntax %test-comp1body
  (syntax-rules (T)
    ((%test-comp1body r expr T)
     (progn
       (if (%test-on-test-begin r)
	   (progn
	     (%test-evaluate-with-catch expr)
             (test-result-set! r 'actual-value T)
             (%test-on-test-end r T) ))
       (%test-report-result) ))
    ((%test-comp1body r expr)
     (with ((res (gensym "RES-")))
       (progn
         (when (%test-on-test-begin r)
           (let ((res (%test-evaluate-with-catch expr)))
             (test-result-set! r 'actual-value res)
             (%test-on-test-end r res) ))
         (%test-report-result) )))))

#|(cond-expand
 ((or kawa mzscheme)
  ;; Should be made to work for any Scheme with syntax-case
  ;; However, I haven't gotten the quoting working.  FIXME.
  (define-syntax test-end
    (lambda (x)
      (syntax-case (list x (list 'quote (%test-source-line2 x))) ()
	(((mac suite-name) line)
	 (syntax
	  (%test-end suite-name line)))
	(((mac) line)
	 (syntax
	  (%test-end 'NIL line))))))
  (define-syntax test-assert
    (lambda (x)
      (syntax-case (list x (list 'quote (%test-source-line2 x))) ()
	(((mac tname expr) line)
	 (syntax
	  (let* ((r (test-runner-get))
		 (name tname))
	    (test-result-alist! r (cons (cons 'test-name tname) line))
	    (%test-comp1body r expr))))
	(((mac expr) line)
	 (syntax
	  (let* ((r (test-runner-get)))
	    (test-result-alist! r line)
	    (%test-comp1body r expr)))))))
  (define-for-syntax (%test-comp2 comp x)
    (syntax-case (list x (list 'quote (%test-source-line2 x)) comp) ()
      (((mac tname expected expr) line comp)
       (syntax
	(let* ((r (test-runner-get))
	       (name tname))
	  (test-result-alist! r (cons (cons 'test-name tname) line))
	  (%test-comp2body r comp expected expr))))
      (((mac expected expr) line comp)
       (syntax
	(let* ((r (test-runner-get)))
	  (test-result-alist! r line)
	  (%test-comp2body r comp expected expr))))))
  (define-syntax test-eqv
    (lambda (x) (%test-comp2 (syntax eqv?) x)))
  (define-syntax test-eq
    (lambda (x) (%test-comp2 (syntax eq?) x)))
  (define-syntax test-equal
    (lambda (x) (%test-comp2 (syntax equal?) x)))
  (define-syntax test-approximate ;; FIXME - needed for non-Kawa
    (lambda (x)
      (syntax-case (list x (list 'quote (%test-source-line2 x))) ()
      (((mac tname expected expr error) line)
       (syntax
	(let* ((r (test-runner-get))
	       (name tname))
	  (test-result-alist! r (cons (cons 'test-name tname) line))
	  (%test-comp2body r (%test-approximimate= error) expected expr))))
      (((mac expected expr error) line)
       (syntax
	(let* ((r (test-runner-get)))
	  (test-result-alist! r line)
	  (%test-comp2body r (%test-approximimate= error) expected expr))))))))
  (:else))|#


(define-syntax test-end
  (syntax-rules ()
    ((test-end)
     (%test-end 'NIL '()))
    ((test-end suite-name)
     (%test-end suite-name '()))))

(define-syntax test-assert
  (syntax-rules ()
    ((test-assert tname test-expression)
     (with ((r (gensym "R-")))
       (let* ((r (test-runner-get))
              (name tname) )
         (declare (ignorable name))
         (test-result-alist! r '((test-name . tname)))
         (%test-comp1body r test-expression) )))
    ((test-assert test-expression)
     (with ((r (gensym "R-")))
       (let* ((r (test-runner-get)))
         (test-result-alist! r '())
         (%test-comp1body r test-expression) )))))

(define-syntax %test-comp2
  (syntax-rules ()
    ((%test-comp2 comp tname expected expr)
     (let* ((r (test-runner-get))
            (name tname) )
       (declare (ignorable name))
       (test-result-alist! r (list (cons 'test-name tname)))
       (%test-comp2body r comp expected expr) ))
    ((%test-comp2 comp expected expr)
     (let* ((r (test-runner-get)))
       (test-result-alist! r '())
       (%test-comp2body r comp expected expr) ))))

(define-syntax test-equal
  (syntax-rules ()
    ((test-equal . rest)
     (%test-comp2 equal? . rest) )))

(define-syntax test-eqv
  (syntax-rules ()
    ((test-eqv . rest)
     (%test-comp2 eqv? . rest) )))

(define-syntax test-eq
  (syntax-rules ()
    ((test-eq . rest)
     (%test-comp2 eq? . rest) )))

(define-syntax test-approximate
  (syntax-rules ()
    ((test-approximate tname expected expr error)
     (%test-comp2 (%test-approximimate= error) tname expected expr) )
    ((test-approximate expected expr error)
     (%test-comp2 (%test-approximimate= error) expected expr) )))

#|(cond-expand
 (guile
  (define-syntax %test-error
    (syntax-rules ()
      ((%test-error r etype expr)
       (%test-comp1body r (catch 'T (lambda () expr) (lambda (key . args) 'T)))))))
 (mzscheme
  (define-syntax %test-error
    (syntax-rules ()
      ((%test-error r etype expr)
       (%test-comp1body r (with-handlers (((lambda (h) 'T) (lambda (h) 'T)))
					 (let ()
					   (test-result-set! r 'actual-value expr)
					   'NIL)))))))
 (chicken
  (define-syntax %test-error
    (syntax-rules ()
      ((%test-error r etype expr)
        (%test-comp1body r (condition-case expr (ex () 'T)))))))
 (kawa
  (define-syntax %test-error
    (syntax-rules ()
      ((%test-error r etype expr)
       (let ()
	 (if (%test-on-test-begin r)
	     (let ((et etype))
	       (test-result-set! r 'expected-error et)
	       (%test-on-test-end r
				  (try-catch
				   (let ()
				     (test-result-set! r 'actual-value expr)
				     'NIL)
				   (ex <java.lang.Throwable>
				       (test-result-set! r 'actual-error ex)
				       (cond ((and (instance? et <gnu.bytecode.ClassType>)
						   (gnu.bytecode.ClassType:isSubclass et <java.lang.Throwable>))
					      (instance? ex et))
					     (:else 'T)))))
	       (%test-report-result))))))))
 ((and srfi-34 srfi-35)
  )
 (srfi-34
 (define-syntax %test-error
  (syntax-rules (cl:t)
    ((%test-error r etype expr)
     (%test-comp1body r (guard (ex (:else 'T)) expr))))))
 (:else
  (define-syntax %test-error
    (syntax-rules ()
      ((%test-error r etype expr)
       (begin
	 ((test-runner-on-test-begin r) r)
	 (test-result-set! r 'result-kind 'skip)
	 (%test-report-result)))))))|#

(define-syntax %test-error
  (syntax-rules (T)
    ((%test-error r T expr)
     (%test-comp1body
      r
      (guard (ex (:else 'T))
             expr )
      T))
    ((%test-error r etype expr)
     (%test-comp1body
      r
      (guard (ex ((condition-type? etype)
                    (and (condition? ex) (condition-has-type? ex etype)) )
                   ((procedure? etype)
                    (funcall etype ex) )
                   ((equal? etype 'T)
                    'T )
                   (:else 'T) )
           expr )))))

;;; cl style condition system
#|(define-syntax %test-error
  (syntax-rules (T)
    ((%test-error r T expr)
     (%test-comp1body r (handler-case expr
                          (cl:error () 'T))))
    ((%test-error r etype expr)
     (%test-comp1body r (handler-case expr
                          (etype ()))))))|#

#|(cond-expand
 ((or kawa mzscheme)

  (define-syntax test-error
    (lambda (x)
      (syntax-case (list x (list 'quote (%test-source-line2 x))) ()
	(((mac tname etype expr) line)
	 (syntax
	  (let* ((r (test-runner-get))
		 (name tname))
	    (test-result-alist! r (cons (cons 'test-name tname) line))
	    (%test-error r etype expr))))
	(((mac etype expr) line)
	 (syntax
	  (let* ((r (test-runner-get)))
	    (test-result-alist! r line)
	    (%test-error r etype expr))))
	(((mac expr) line)
	 (syntax
	  (let* ((r (test-runner-get)))
	    (test-result-alist! r line)
	    (%test-error r 'T expr))))))))
 (:else
  ))|#

(define-syntax test-error
  (syntax-rules (T)
    ((test-error name etype expr)
     (with ((r (gensym "R-")))
       (let* ((r (test-runner-get)))
         (declare (ignorable r))
         (test-assert name (%test-error r etype expr)) )))
    ((test-error T expr)
     (with ((r (gensym "R-")))
       (let* ((r (test-runner-get)))
         (declare (ignorable r))
         (test-assert (progn (%test-error r T expr) T)) )))
    ((test-error etype expr)
     (with ((r (gensym "R-")))
       (let* ((r (test-runner-get)))
         (declare (ignorable r))
         (test-assert (%test-error r etype expr)) )))
    ((test-error expr)
     (with ((r (gensym "R-")))
       (let* ((r (test-runner-get)))
         (declare (ignorable r))
         (test-assert (%test-error r 'T expr)) )))))

(define-syntax test-with-runner
  (syntax-rules ()
    ((test-with-runner runner form ***)
     (let ((saved-runner (test-runner-current)))
       (dynamic-wind
        (lambda () (test-runner-current runner))
        (lambda () form ***)
        (lambda () (test-runner-current saved-runner)) )))))

(define-function (test-apply first . rest)
  (if (test-runner? first)
      (test-with-runner first (apply #'test-apply rest))
      (let ((r (test-runner-current)))
	(if r
	    (let ((run-list (%test-runner-run-list r)))
	      (cond ((null? rest)
		     (%test-runner-run-list! r (nreverse run-list))
		     (funcall first) ) ;; actually apply procedure thunk
		    (:else
		     (%test-runner-run-list!
		      r
		      (if (eq? run-list 'T) (list first) (cons first run-list)) )
		     (apply #'test-apply rest)
		     (%test-runner-run-list! r run-list) )))
	    (let ((r (test-runner-create)))
	      (test-with-runner r (apply #'test-apply first rest))
	      (funcall (test-runner-on-final r) r) )))))

;;; Predicates

(define-function (%test-match-nth n count)
  (let ((i 0))
    (lambda (runner)
      (declare (ignore runner))
      (set! i (+ i 1))
      (and (>= i n) (< i (+ n count))) )))

(define-syntax test-match-nth
  (syntax-rules ()
    ((test-match-nth n)
     (test-match-nth n 1) )
    ((test-match-nth n count)
     (%test-match-nth n count) )))

(define-function (%test-match-all . pred-list)
  (lambda (runner)
    (let ((result 'T))
      (srfi-5:let loop ((l pred-list))
	(if (null? l)
	    result
	    (begin
	      (if (not (funcall (car l) runner))
		  (set! result 'NIL) )
	      (loop (cdr l)) ))))))

(define-syntax test-match-all
  (syntax-rules ()
    ((test-match-all pred ***)
     (%test-match-all (%test-as-specifier pred) ***) )))

(define-function (%test-match-any . pred-list)
  (lambda (runner)
    (let ((result 'NIL))
      (srfi-5:let loop ((l pred-list))
	(if (null? l)
	    result
	    (begin
	      (if (funcall (car l) runner)
		  (set! result 'T) )
	      (loop (cdr l)) ))))))

(define-syntax test-match-any
  (syntax-rules ()
    ((test-match-any pred ***)
     (%test-match-any (%test-as-specifier pred) ***) )))

;; Coerce to a predicate function:
(define-function (%test-as-specifier specifier)
  (cond ((procedure? specifier) specifier)
	((integer? specifier) (test-match-nth 1 specifier))
	((string? specifier) (test-match-name specifier))
	(:else
	 (error "not a valid test specifier") )))

(define-syntax test-skip
  (syntax-rules ()
    ((test-skip pred ***)
     (let ((runner (test-runner-get)))
       (%test-runner-skip-list! runner
				  (cons (test-match-all (%test-as-specifier pred)  ***)
					(%test-runner-skip-list runner) ))))))

(define-syntax test-expect-fail
  (syntax-rules ()
    ((test-expect-fail pred ***)
     (let ((runner (test-runner-get)))
       (%test-runner-fail-list! runner
				  (cons (test-match-all (%test-as-specifier pred)  ***)
					(%test-runner-fail-list runner) ))))))

(define-function (test-match-name name)
  (lambda (runner)
    (equal? name (test-runner-test-name runner)) ))

(define-function (test-read-eval-string string)
  (with-input-from-string (port string)
    (let ((form (read port nil +eof+)))
      (if (eof-object? (read-char port nil +eof+))
          (eval form)
          (error "(not at eof)")))))
