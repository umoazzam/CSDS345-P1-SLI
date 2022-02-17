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
      ((not (list? statement)) (if (null? (checkDeclaredVariables statement state))
                              (error 'error "variable use before assigning")
                              (checkDeclaredVariables statement state)))
      ((eq? (operator statement) '+) (+ (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '-) (cond
                                       ((null? (cddr statement)) (- 0 (M_value (leftOperand statement) state)))
                                       (else(- (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))))
      ((eq? (operator statement) '*) (* (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '/) (quotient (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '%) (remainder (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '>) (> (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '>=) (>= (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '<) (< (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '<=) (<= (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '!=) (not (eq? (M_value (leftOperand statement) state) (M_value (rightOperand statement) state))))
      ((eq? (operator statement) '==) (eq? (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      (else (error 'badop "Bad operator")))))

(define M_boolean
  (lambda (condition state)
    (cond
      ((eq? 'true condition) #t)
      ((eq? 'false condition) #f)
      (else (M_value condition state)))))

;; ***********************************************************
; States and function states

(define M_state
  (lambda (statement state)
    (cond
      ((eq? (function statement) 'return) (M_state-return (returnExpression statement) state))
      ((eq? (function statement) 'var) (M_state-declaration statement state))
      ((eq? (function statement) '=) (M_state-assign (leftOperand statement) (expression statement) state))
      ((eq? (function statement) 'if)
       (cond
         ((eq? (length statement) 4)(M_state-ifElse (ifCondition statement)(statement1 statement) (statement2 statement) state))
         (else (M_state-if (ifCondition statement)(statement1 statement) state))))
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

(define M_state-if
  (lambda (condition ifStatement state)
    (cond
      ((M_boolean condition state) (M_state ifStatement state))
      (else state))))

(define M_state-ifElse
  (lambda (condition ifStatement elseStatement state)
    (cond
      ((M_boolean condition state) (M_state ifStatement state))
      (else (M_state elseStatement state)))))
    

;; ***********************************************************

; checks if variable is already declared
(define declared?
  (lambda (var variables)
    (cond
      ((null? variables) #f)
      ((eq? var (car variables)) #t)
      (else (declared? var (cdr variables))))))

; looks through all declared variables
(define checkDeclaredVariables
  (lambda (var state)
    (cond
      ((null? (declaredVariables state)) (error 'error "undeclared variable"))
      ((eq? var (firstVariable state)) (firstValue state))
      (else (checkDeclaredVariables var (restof state))))))

; insert newly declared variable's value
(define insert
  (lambda (var val state)
    (cond
      ((not (declared? var (declaredVariables state))) (error 'error "undeclared variable"))
      ((eq? var (firstVariable state)) (cons (declaredVariables state) (cons (cons val (remainingValues state)) '())))
      (else (list (cons (firstVariable state) (declaredVariables (insert var val (restof state))))
                  (cons (firstValue state) (declaredValues (insert var val (restof state)))))))))


;; ***********************************************************
; Abstractions

; looking for "var", "if", "while", "=", etc
(define function car)

(define expression caddr)
(define returnExpression cadr)

; Follows (- 1 2) format
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

(define ifCondition cadr)
(define statement1 caddr)
(define statement2 cadddr)