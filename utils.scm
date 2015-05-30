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

;;;;
;; compose
;;  Compose two functions
;;  (f . g)
;;
;; @param f - function
;; @param g - function
;; @return function
(define (compose f g)
  (lambda (x) (f (g x))))

;;;;
;; init
;;  Haskell-like init
;;  eqv to drop-right lst 1
;;
;; @param lst
;; @return all but last of list
(define (init lst)
  (drop-right lst 1))

;;;;
;; string-split
;;  Splits a string based on char
;;
;; @param char - char to split string by
;; @return list - list of string partitions
(define (string-split str char)
  (if (or (null? str) (string=? str ""))
      (list "")
      (let loop ((str-list (string->list str))
                 (current '())
                 (splits '()))
        (if (null? str-list)
            (let ((lst (map list->string (append splits (list current)))))
              (if (string=? (last lst) "")
                  (init lst)
                  lst))
            (if (char=? (car str-list) char)
                (loop (cdr str-list) '() (append splits (list current)))
                (loop (cdr str-list) (append current (list (car str-list))) splits))))))

  
'done

