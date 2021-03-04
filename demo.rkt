#lang racket
(require "simpleParser.rkt")

;first draft - IRL

(define interpret
  (lambda (filename)
    (evaluate-tree (parser filename))))

;evaluates the tree by calling evaluate-line
(define evaluate-tree
  (lambda (tree)
    (cond
      (evaluate-line (first-line tree) tree '()))))

;gets the first line of the tree
(define first-line car)

;if the state is a number, this means there was a return statement inside an if/while statement
;evaluated each line of the tree
(define evaluate-line
  (lambda (line tree state)
    (cond
      ((null? tree) state) ;maybe skips last line, not sure tho
      ((number? state) state)
      ((eq? (line-type line) 'var) (evaluate-line (next-line tree) (cdr tree) (declaration (name line) line state)))
      ((eq? (line-type line) '=) (evaluate-line (next-line tree) (cdr tree) (assignment (name line) (expression line) state)))
      ((eq? (line-type line) 'if) (evaluate-line (next-line tree) (cdr tree) (if-statement (condition line) (then-statement line) (else-statement line) state)))
      ((eq? (line-type line) 'while) (evaluate-line (next-line tree) (cdr tree) (while-statement (condition line) (then-statement line) state)))
      ((eq? (line-type line) 'return) (return (return-expression line))))))

;gets the variable name in a declaration/assignment statement
(define name cadr)
;gets the expression in the assignment statement
(define expression cddr)
;gets the condition in a if or while statement
(define condition cadr)
;gets the then statement in a if or while statement
(define then-statement caddr)
;gets the else statement in a if statement
(define else-statement cadddr)
;gets the return expression in a return statement
(define return-expression cadr)
;gets the type of the line, the first element in the line
(define line-type car)
;gets the next line in the tree
(define next-line cdar)

;adds a variable and its value to state, if the value has been declared, but not assigned, its corresponding value is null
(define Add_M_state
  (lambda (name value state)
    (cons (cons name (car state)) (cons (cons value (car (cdr state))) '()))))

;does not work 
;(define Remove_M_state
  ;(lambda (name declare-list value-list)
   ;(cond
      ;((null? declare-list) '())
      ;((eq? (car declare-list) name) (cons (cdr declare-list) (cons (cdr value-list) '())))
      ;(else (Remove_M_state name (cons (car declare-list) (cdr declare-list)) (cons (car value-list) (cdr value-list)))))))

;placeholder
;returns state
(define M_state
  (lambda (expression state)
    (cond
      ((null? expression) state)
      ((list? (car expression)) (M_state (cdr expression) (M_state (car expression) state)))
      ((eq? (car expression) 'var) (declaration (name (car expression)) (car expression)))
      ((eq? (car expression) '=) (assignment (name (car expression)) (expression (car expression)) state))
      (else state))))


;placeholder
(define M_boolean
  (lambda (expression state)
    (cond
      ((eq? (car expression) '&&) (and (M_boolean (cdar expression)) (M_boolean (cddar expression))))
      ((eq? (car expression) '||) (or (M_boolean (cdar expression)) (M_boolean (cddar expression))))
      ((eq? (car expression) '!) (not (M_boolean (cdar expression))))
      ((eq? (car expression) '==) (= (M_value (cdar expression)) (M_value (cddar expression))))
      ((eq? (car expression) '!=) (not (= (M_value (cdar expression)) (M_value (cddar expression)))))
      ((eq? (car expression) '<) (< (M_value (cdar expression)) (M_value (cddar expression))))
      ((eq? (car expression) '<=) (<= (M_value (cdar expression)) (M_value (cddar expression))))
      ((eq? (car expression) '>) (> (M_value (cdar expression)) (M_value (cddar expression))))
      ((eq? (car expression) '>=) (>= (M_value (cdar expression)) (M_value (cddar expression))))
      (else (error "Invalid")))))
    
(define M_value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((eq? (operator expression) '+) (+ (M_value (car (cdr expression))) (M_value (car (cdr (cdr expression))))))
      ((eq? (operator expression) '/) (quotient (M_value (car (cdr expression))) (M_value (car(M_value (* '(+ 4 3) '(- 2 1))) (cdr (cdr expression))))))
      ((eq? (operator expression) '%) (remainder (M_value (leftoperand expression)) (M_value (rightoperand expression))))
      ((eq? (operator expression) '-) (- (M_value (leftoperand expression)) (M_value (rightoperand expression))))
      ((eq? (operator expression) '*) (* (M_value (leftoperand expression)) (M_value (rightoperand expression))))
      (else (get_from_state expression (car state) (cdar state)))))) ;not sure if this is the best way to get variable values

(define operator (lambda (expression) (car expression)))
(define leftoperand cadr) 
(define rightoperand caddr)

(define get_from_state
  (lambda (name declare-list value-list)
    (cond
      ((null? declare-list) (error "Not declared"))
      ((and (eq? name (car declare-list)) (not (eq? (car value-list) 'null))) (car value-list))
      (else (get_from_state name (cdr declare-list) (cdr value-list))))))
      


;if value a non number/not a boolean, its undeclared
(define declaration
  (lambda (name line state)
    (if (null? (cddr line))
        (Add_M_state name null (M_state expression state))
        (Add_M_state name (M_value expression state) (M_state expression state)))))

(define assignment
  (lambda (name expression state)
    (if (is_declared name (car state)) ;if has been declared
        (M_state name (M_value expression state) (M_state expression state))
        (error "Not Declared"))))

;checks if the variable name has been declared before assignment
;gets the declare-list from the state (first sub list)
(define is_declared
  (lambda (name declare-list)
    (cond
      ((null? declare-list) #f)
      ((eq? name (car declare-list)) #t)
      (else (is_declared name (cdr declare-list))))))

;determines if the variable has been assigned
;gets the declare-list from the state (first sub list) and the value-list from the state (second sublist)
(define is_assigned
  (lambda (name declare-list value-list)
    (cond
      ((null? declare-list) error "Not Declared")
      ((eq? (car declare-list) name) (not (eq? (car value-list) 'null)))
      (else (is_assigned name (cdr declare-list) (cdr value-list))))))

(define return
  (lambda (expression state)
    (M_value expression state)))

(define if-statement
  (lambda (condition then-statement else-statement state)
    (if (M_boolean condition (M_state condition state))
        (M_state then-statement (M_state condition state))
        (M_state else-statement (M_state condition state)))))

(define while-statement
  (lambda (condition body-statement state)
    (if (M_boolean condition (M_state condition state))
        (while-statement condition body-statement (M_state body-statement (M_state condition state)))
        state)))
