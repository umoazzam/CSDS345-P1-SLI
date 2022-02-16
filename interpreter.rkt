#lang racket

(require "simpleParser.rkt")

;; ***********************************************************
; Used to run txt files
; (interpret "name.txt") will convert to the parse tree and begin running our interpreter

(define interpret
  (lambda (filename)
    (evaluateParseTree (parser filename) '(() ()))))

(define evaluateParseTree
  (lambda (parseTree state)
    (cond
      ((null? parseTree) state)
      (else(evaluateParseTree (cdr parseTree) (M_state (car parseTree) state))))))

;; ***********************************************************
; Basic types: int, boolean
(define M_value
  (lambda (statement state)
    (cond
      ((number? statement) statement)
      ((not (list? statement))
            (cond
              ((null? (lookup statement state)) (error 'error "undeclared variable"))
              (else(lookup statement state))))
      ((eq? '+ (operator statement)) (+ (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '-) (- (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '*) (* (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '/) (quotient (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '%) (remainder (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      (else (error 'badop "Bad operator")))))

(define M_boolean
  (lambda (condition state)
    (cond
      ((eq? 'true condition) #t)
      ((eq? 'false condition) #f)
      (else M_value condition state))))

;; ***********************************************************
; States and function states

(define M_state
  (lambda (statement state)
    (cond
      ((eq? (function statement) 'return) (M_state-return (returnExpression statement) state))
      ((eq? (function statement) 'var) (M_state-declaration statement state))
      ((eq? (function statement) '=) (M_state-assign (leftOperand statement) (expression statement) state))
      (else (M_value statement state)))))

(define M_state-return
  (lambda (statement state)
    (cond
      (else (M_value statement state)))))

(define M_state-declaration
  (lambda (statement state)
    (cond
      ((declared? (newVariable statement) (declaredVariables state)) (error 'error "variable is already defined"))
      ((null? (cddr statement)) (list (cons (newVariable statement) (declaredVariables state)) (cons '() (declaredValues state))))
      (else (list(cons (newVariable statement) (declaredVariables state)) (cons (M_value (newValue statement) state) (declaredValues state)))))))

(define M_state-assign
  (lambda (variable statement state)
    (cond
      ((declared? variable (declaredVariables state)) (insert variable (M_value statement state) (remove variable state)))
      (else(error 'error "undeclared variable")))))

;; ***********************************************************

(define declared?
  (lambda (var variables)
    (cond
      ((null? variables) #f)
      ((eq? var (car variables)) #t)
      (else (declared? var (cdr variables))))))

(define lookup
 (lambda (var state)
    (cond
      ((null? (declaredVariables state)) (error 'error "undeclared variable"))
      ((eq? var (firstVariable state)) (firstValue state))
      (else (lookup var (cdr state))))))

(define insert
  (lambda (var val state)
    (cond
      ((not (declared? var (declaredVariables state))) (error 'error "undeclared variable"))
      ((eq? var (firstVariable state)) (cons (declaredVariables state) (cons (cons val (remainingValues state)) '())))
      (else (list (cons (firstVariable state) (declaredVariables (insert var val (restof state))))
                  (cons (firstValue state) (declaredValues (insert var val (restof state)))))))))


;; ***********************************************************
; Abstractions

(define function car)
(define expression caddr)
(define returnExpression cadr)

(define operator car)
(define leftOperand cadr)
(define rightOperand caddr)

(define newVariable cadr)
(define newValue caddr)
(define declaredVariables car)
(define declaredValues cadr)

(define firstVariable caar)
(define firstValue caadr)
(define remainingValues cdadr)

(define restof
  (lambda (state)
    (cons (cdar state) (cons (cdadr state) '()))))