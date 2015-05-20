;; Load src
(define (src-load src)
  (set! load/suppress-loading-message? #t)
  (with-working-directory-pathname (directory-namestring (current-load-pathname)) (lambda () (load src)))
  (set! load/suppress-loading-message? #f))

;; Load options
(define (option-load option)
  (set! load/suppress-loading-message? #t)
  (load-option option)
  (set! load/suppress-loading-message? #f))

;; Load format
(option-load 'format)

;;;;
;; when
;;  Executes expressions iff pred is true
;; 
;; @param pred - Predicate
;; @param exp ... - Expressions
(define-syntax when
  (syntax-rules ()
    ((when pred exp ...)
      (if pred (begin exp ...)))))

;;;;
;; funcall
;;  Lisp-like funcall. Applies args to p.
;;
;; @param p - Procedure
;; @param args ... - Arguments (Expressions)
(define-syntax funcall
  (syntax-rules ()
    ((funcall p args ...)
      (apply (if (procedure? p) p (eval p (nearest-repl/environment))) (list args ...)))))

;;;;
;; promisify
;;  Ensures that object is a promise
;;
;; @param object
;; @return delayed object
(define (promisify object)
  (if (promise? object)
    object
    (delay object)))

;;;;
;; forcify
;;  Ensures that a promise has been forced
;;
;; @param object
;; @return forced object
(define (forcify object)
  (if (promise? object)
    (force object)
    object))

;;;;
;; messagify
;;  Transforms an object into a string
;;
;; @param object
;; @return String representation of object
(define (messagify object)
  (with-output-to-string (lambda () (display object))))

;;;;
;; call-capture-errors
;;  Calls a procedure `proc' and captures errors
;;
;; @param proc
;; @return return of proc
(define (call-capture-errors proc)
  (when standard-error-hook
    (warn "Cannot definitively capture errors if standard-error-hook is bound."))
  (call-with-current-continuation
    (lambda (x)
      (fluid-let ((standard-error-hook x))
        (proc)))))

;;;;
;; Y
;;  Fixed-point combinator Y
;;
;; @param f - proc
;; @return proc
(define Y
  (lambda (f)
    ((lambda (x)
       (f (lambda y
          	(apply (x x) y))))
     (lambda (x)
       (f (lambda y
          	(apply (x x) y)))))))
