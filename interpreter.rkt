#lang racket

(define (josh-eval expr)
  (r-eval expr globals))

(define (r-eval expr env)
  (cond
    [(number? expr) expr]
    [(string? expr) expr]
    [(boolean? expr) expr]
    [(symbol? expr) (env-lookup expr env)]
    [(list? expr) (r-eval-list expr env)]
    [else (evaluation-error)]))

(define (r-eval-list expr env)
  (define head (car expr))
  (define rest (cdr expr))
  (cond
    [(equal? head 'quote) rest]
    [(equal? head 'lambda) (r-lambda rest env)]
    [(equal? head 'define) (r-define rest env)]
    [(equal? head 'begin) (r-begin rest env)]
    [(equal? head 'if) (r-if rest env)]
    [else (r-apply expr env)])
  )

(define (r-apply expr env)
  (define function (car expr))
  (define args (cdr expr))
  (apply
   (r-eval function env)
   (map (lambda (x) (r-eval x env)) args)))

(define (r-begin expr env)
  (last (map (r-eval expr env))))

(define (r-define expr env)
  (arg-check expr 2)
  (define sym (car expr))
  (define val (cadr expr))
  (define-var sym (r-eval val env) env))

(define (r-if expr env)
  (arg-check expr 3)
  (define conditional (car expr))
  (define true-block (cadr expr))
  (define else-block (caddr expr))
  (if (r-eval conditional env)
      (r-eval true-block env)
      (r-eval else-block env)))

(define (r-lambda expr env)
  (arg-check expr 2)
  (define arg-names (car expr))
  (define body (cadr expr))
  (lambda args
    (define closure (make-closure env))
    (arg-check args (length arg-names))
    (for ([arg-name arg-names]
          [arg args])
      (define-var arg-name arg closure))
    (r-eval body closure)
    )
  )

(define namespace (make-base-namespace))

(define (make-globals)
  (define (helper sym)
    `(,sym . ,(eval sym namespace)))
  (make-hash
   (map helper
        '(+ - / * expt car cdr cons = not print))))

(define globals (make-globals))

(define (make-closure env)
  (hash-copy env))

(define (define-var sym val env)
  (hash-set! env sym val))

(define (env-lookup sym env)
  (hash-ref env sym))

(define (arg-check recieved-expr expected)
  (if (not (= expected (length recieved-expr)))
      (error "incorrect arg count")
      (void)))  
  
(define (evaluation-error)
  (error "evaluation error"))

(define (not-implemented)
  (error "not-implemented"))

(for ([i (in-naturals)]) (print (josh-eval (read))))