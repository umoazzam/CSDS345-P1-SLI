#lang racket

(require "simpleParser.rkt")

(define interpret
  (lambda (filename)
    (evaluateParseTree (parser filename) '())))

(define evaluateParseTree
  (lambda (parseTree state)
    (cond
      ((null? parseTree) state)
      (else(evaluateParseTree (cdr parseTree) (M_state (car parseTree) state))))))

(define M_state
  (lambda (statement state)
    (cond
      ((eq? (car statement) 'return) (M_state-return (cadr statement) state))
      (else (M_value statement state)))))

(define M_value
  (lambda (expression state)
    (cond
    ((number? expression) expression)
    ((eq? (car (cdr expression)) '+) (+ (M_value (car expression) state) (M_value (caddr expression) state)))
    ((eq? (car (cdr expression)) '-) (- (M_value (car expression) state) (M_value (caddr expression) state)))
    ((eq? (car (cdr expression)) '*) (* (M_value (car expression) state) (M_value (caddr expression) state)))
    ((eq? (car (cdr expression)) '/) (quotient (M_value (car expression) state) (M_value (caddr expression) state)))
    ((eq? (car (cdr expression)) '%) (remainder (M_value (car expression) state) (M_value (caddr expression) state)))
    (else (error 'badop)))))

(define M_boolean
  (lambda (condition state)
    (cond
      ((eq? 'true condition) #t)
      ((eq? 'false condition) #f)
      (else M_value condition state))))

(define M_state-return
  (lambda (statement state)
    (cond
      (else (M_value statement state)))))