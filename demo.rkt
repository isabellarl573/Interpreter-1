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

(define Remove_M_state* ;have to pass '(() ()) into savedlist, savelist holds the part of the state that is not equal to the name
  (lambda (name savedlist state)
    (cond
      ((null? (car state)) state) ; if the list of names in state is empty (dont know if it should just check if state is null
      ((eq? name (car (car state))) cons (cons (car savedlist) (cdr (car state))) (cons (cdr savedlist) (cdr (cdr state)))) ;checks if the name is eqaual to the car and if it does it skips it by consing the saved list and cdr of state
      (else (Remove_M_state* name cons (cons (car savedlist) (car (car state))) (cons (cdr savedlist) (car (cdr state))) state))))) ;if not equal run recursion but add the state part into savedlist

;placeholder
;returns state
(define M_state
  (lambda (expression state)
    (cond
      ((null? expression) state)
      ((list? (car expression)) (M_state (cdr expression) (M_state (car expression) state)))
      ((eq? (car expression) 'var) (declaration (name expression) expression state))
      ((eq? (car expression) '=) (assignment (name expression) (expression expression) state))
      (else state))))


;tested
(define M_boolean
  (lambda (expression state)
    (cond
      ((not (list? expression)) (M_value expression state))
      ((eq? (operator expression) '&&) (and (M_boolean (leftoperand expression) state) (M_boolean (rightoperand expression) state)))
      ((eq? (operator expression) '||) (or (M_boolean (leftoperand expression) state) (M_boolean (rightoperand expression) state)))
      ((eq? (operator expression) '!) (not (M_boolean (leftoperand expression) state)))
      ((eq? (operator expression) '==) (= (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '!=) (not (= (M_value (leftoperand expression) state) (rightoperand (caddr expression) state))))
      ((eq? (operator expression) '<) (< (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '<=) (<= (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '>) (> (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '>=) (>= (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      (else (error "Invalid")))))
      
;tested
(define M_value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((not (pair? expression)) (get_from_state expression (car state) (cadr state)))
      ((eq? (operator expression) '+) (+ (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '-) (- (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (M_value (leftoperand expression) state) (M_value (rightoperand expression) state)))
      (else (error "Invalid"))))) ;not sure if this is the best way to get variable values

(define operator car)
(define leftoperand cadr) 
(define rightoperand caddr)

;tested
(define get_from_state
  (lambda (name declare-list value-list)
    (cond
      ((null? declare-list) (error "Not declared"))
      ((and (eq? name (car declare-list)) (not (eq? (car value-list) 'null))) (car value-list))
      (else (get_from_state name (cdr declare-list) (cdr value-list))))))
      

;tested, but not when there is a nested declaration/assignment
;if value a non number/not a boolean, its undeclared
(define declaration
  (lambda (name line state)
    (if (null? (cddr line))
        (Add_M_state name 'null state)
        (Add_M_state name (M_value (caddr line) state) (M_state (caddr line) state)))))

;tested, but does not remove the declared variable and value
(define assignment
  (lambda (name expression state)
    (if (is_declared name (car state)) ;if has been declared
        (Add_M_state name (M_value expression state) (M_state expression state))
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
    (M_value expression (M_state expression state))))

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
