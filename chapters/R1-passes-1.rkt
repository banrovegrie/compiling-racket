#lang racket
;; Here, we shall discuss the following passes:
;;      - uniquify (rename)
;;      - remove complex operations (use let bindings)
;;      - explicate control ()
;;
;; Explicate control results in a new language called C0.

;;-------------------------------------------------------------------;;

;; Definition of R1:
;;      exp := int | (read) | var | (- exp) | (+ exp exp)
;;              | (let [var exp] exp)
;;      R1 := exp

;; uniquify pass
(define (uniquify symtab)
  (lambda (e)
    (match e
      [(Var x) (Var (dict-ref symtab x))]
      [(Int n) (Int n)]
      [(Let x e body) (let* ([new-sym (gensym x)] [new-symtab (dict-set symtab x new-sym)])
                        (Let new-sym ((uniquify-exp symtab) e) ((uniquify-exp new-symtab) body)))]
      [(Prim op es) (Prim op
                          (for/list ([e es])
                            ((uniquify-exp symtab) e)))])))

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp (hash)) e))]))

;;-------------------------------------------------------------------;;

;; We are still in R1
;; rco-atom : exp -> exp * (var * exp) list
(define (rco-atom e)
  (match e
    [(Var x) (values (Var x) '())]
    [(Int n) (values (Int n) '())]
    [(Let x rhs body)
     (define new-rhs (rco-exp rhs))
     (define-values (new-body body-ss) (rco-atom body))
     (values new-body (append `((,x . ,new-rhs)) body-ss))]
    [(Prim op es)
     (define-values (new-es sss) (for/lists (l1 l2) ([e es]) (rco-atom e)))
     (define ss (append* sss))
     (define tmp (gensym 'tmp))
     (values (Var tmp) (append ss `((,tmp . ,(Prim op new-es)))))]))

(define (make-lets bs e)
  (match bs
    [`() e]
    [`((,x . ,e^) . ,bs^) (Let x e^ (make-lets bs^ e))]))

;; rco-exp : exp -> exp
(define (rco-exp e)
  (match e
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    [(Let x rhs body) (Let x (rco-exp rhs) (rco-exp body))]
    [(Prim op es)
     (define-values (new-es sss) (for/lists (l1 l2) ([e es]) (rco-atom e)))
     (make-lets (append* sss) (Prim op new-es))]))

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [(Program info e) (Program info (rco-exp e))]))

;; Grammar after remove complex operations:
;;      atm := var | int
;;      exp := atm | (read) | (-atm ) | (+ atm atm)
;;              | (let ([var exp]) exp)
;;      R1 := exp

;;-------------------------------------------------------------------;;

;; explicate control pass

;; explicate-tail : exp -> tail * var list
(define (explicate-tail exp)
  (match exp
    [(Var x) (values (Return (Var x)) '())]
    [(Int n) (values (Return (Int n)) '())]
    [(Let lhs rhs body) (let*-values ([(body-c0 body-vars) (explicate-tail body)]
                                      [(new-tail new-rhs-vars) (explicate-assign lhs rhs body-c0)])
                          (values new-tail (append new-rhs-vars body-vars)))]
    [(Prim op es) (values (Return (Prim op es)) '())]))

;;  explicate-assign : exp -> var -> tail -> tail * var list
(define (explicate-assign r1exp v c)
  (match r1exp
    [(Let x e body)
     (define-values (tail let-binds) (explicate-assign body v c))
     (define-values (tail^ let-binds^) (explicate-assign e (Var x) tail))
     (values tail^ (cons x (append let-binds let-binds^)))]
    [else (values (Seq (Assign v r1exp) c) '())]))

;; explicate-control: R1 -> C0
(define (explicate-control p)
  (match p
    [(Program info body) (CProgram info (list (cons 'start (explicate-tail body))))]))

;; Grammer of C0 obtained after explicate control
;;      atm := int | var
;;      exp := atm | (read) | (- atm) | (+ atm atm)
;;      stmt := var = exp;
;;      tail := return exp; | stmt tail
;;      C0 := (label: tail)^+

;;-------------------------------------------------------------------;;
