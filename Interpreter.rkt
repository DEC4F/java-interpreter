; Group 69
; Member: Shihong Ling, Yuhang Li, Stanley Tian

; include parse tree
(load "simpleParser.scm")

; interpret the grammar and calculate output
(define interpret
  (lambda (file)
    (evaluate (parser file) '(() ())) ))

; evaluate the statemet in the parse tree if every statement is evaluated, return the state which will store the output
(define evaluate
  (lambda (parse_tree state)
    (cond
      ((null? parse_tree) state)
      (else (evaluate (cdr parse_tree) (M_state (car parse_tree) state))) )))

; check each type of statement and update the state
(define M_state
  (lambda (statement state)
    (cond
      ((null? statement) (error "Empty statement"))
      ;check if statement
      ((eq? (car statement) 'if)
       (cond
         ((M_boolean (cadr statement) state) (M_state (car (cddr statement)) state))
         ((null? (cdr (cdr (cdr statement)))) state)
         (else (M_state (car (cdddr statement)) state))))
      ;check return statement
      ((eq? (car statement) 'return)
       (cond
         ((not (isBooleanOperation (cadr statement) (car state) (cadr state))) (M_value (cadr statement) state))
         ((M_boolean (cadr statement) state) 'true)
         (else 'false) ))
      ;check while statement
      ((eq? (car statement) 'while) (M_while (cadr statement) (car (cddr statement)) state))
      ((eq? (car statement) 'var) M_declare (cdr statement) state) )))
      
; evaluate the while statement returns state
(define M_while
  (lambda (condition expression state)
    (cond
      ((M_boolean condition state) (M_while condition expression (M_state expression state)))
      (else state) )))

; evaluates the condition, 
(define M_boolean
  (lambda (condition state)
    (cond
      ((eq? condition 'true) #t)
      ((eq? condition 'false) #f)
      ((atom? condition) (find condition (car state) (cadr state)))
      ((eq? (car condition) '==) (eq? (M_value (cadr condition) state) (M_value (car (cddr condition)) state)))
      ((eq? (car condition) '>) (> (M_value (cadr condition) state) (M_value (car (cddr condition)) state)))
      ((eq? (car condition) '<) (< (M_value (cadr condition) state) (M_value (car (cddr condition)) state)))
      ((eq? (car condition) '>=) (>= (M_value (cadr condition) state) (M_value (car (cddr condition)) state)))
      ((eq? (car condition) '<=) (<= (M_value (cadr condition) state) (M_value (car (cddr condition)) state)))
      ((eq? (car condition) '!=) (not (= (M_value (cadr condition) state) (M_value (car (cddr condition)) state))))
      ((eq? (car condition) '||) (or (M_boolean (cadr condition) state) (M_boolean (car (cddr condition)) state)))
      ((eq? (car condition) '&&) (and (M_boolean (cadr condition) state) (M_boolean (car (cddr condition)) state)))
      ((eq? (car condition) '!) (not (M_boolean (cdr condition) state))) )))

; calculate the arithmetic expressions and return the number after calculation
(define M_value
  (lambda (expression state)
    (cond
      ((null? expression) ())
      ((number? expression) expression)
      ((atom? expression) (find expression (car state) (cadr state)))
      ((eq? (car expression) '+) (+ (M_value (cadr expression) state) (M_value (car (cddr expression)) state)))
      ((eq? (car expression) '-) (cond
                                   ((eq? (cddr expression) '()) (- 0 (M_value (cadr expression) state)))
                                   (else (- (M_value (cadr expression) state) (M_value (car (cddr expression)) state))) ))
      ((eq? (car expression) '*) (* (M_value (cadr expression) state) (M_value (car (cddr expression)) state)))
      ((eq? (car expression) '/) (quotient (M_value (cadr expression) state) (M_value (car (cddr expression)) state)))
      ((eq? (car expression) '%) (remainder (M_value (cadr expression) state) (M_value (car (cddr expression)) state)))
      ((number? (car expression)) (car expression))
      ((atom? (car expression)) (find (car expression) (car state) (cadr state)))
      ((list? (car expression)) (M_value (car expression) state)) )))

(define M_declare
  (lambda (statement state)
    (cond
      ; edge case: var already declared
      ((declared? (car statement) (car state)) (error "This variable has already been declared."))
      ; case 1: declare a var but not assign any value
      ((null? (cdr statement)) (add (car statement) '() (car state) (cadr state)))
      ; case 2: declare a var and assign a boolean to it
      ((isBooleanOperation? (cadr statement) (car state) (cadr state))
        (add (car statement) (M_boolean (cadr statement) state) (car state) (cadr state)))
      ; case 3: declare a var and assign the val of another var to it
      ((and (list? (cadr statement)) (eq? (car (cadr statement)) '=))
        (add (car statement) (find (cadr (cadr statement))
                                   (car (M_state (cadr statement) state))
                                   (cadr (M_state (cadr statement) state))) ; find the val associated with the var
                             (car (M_state (cadr statement) state))
                             (cadr (M_state (cadr statement) state)) )) ; add val & name to state lists
      ; case 4: declare a var and assign num or an expression to it
      (else (add (car statement) (M_value (cadr statement) state) (car state) (cadr state))) )))

; check the expression is boolean or numeric, #t if is boolean
(define isBooleanOperation
  (lambda (expression namelist valuelist)
    (cond
      ((null? expression) #f)
      ((number? expression) #f)
      ((boolean? expression) #t)
      ((atom? expression) (isBooleanOperation (find expression namelist valuelist) namelist valuelist))
      ((eq? (car expression) '+) #f)
      ((eq? (car expression) '-) #f)
      ((eq? (car expression) '*) #f)
      ((eq? (car expression) '/) #f)
      ((eq? (car expression) '&&) #t)
      ((eq? (car expression) '||) #t)
      ((eq? (car expression) '!) #t)
      ((eq? (car expression) '>=) #t)
      ((eq? (car expression) '==) #t)
      ((eq? (car expression) '<=) #t)
      ((eq? (car expression) '>) #t)
      ((eq? (car expression) '<) #t)
      ((eq? (car expression) '!=) #t)
      ((eq? (car expression) 'true) #t)
      ((eq? (car expression) 'false) #t)
      ((list? (car expression)) (isBooleanOperation (car expression) namelist valuelist))
      (else #f) )))

; find the value by given name, return error when not declared or not initialized 
(define find
  (lambda (name namelist valuelist)
    (cond
      ((declared? name namelist) (initialized? name namelist valuelist))
      (else (error "error: you need to declare a variable first")) )))

; check if the name is declared
(define declared?
  (lambda (name namelist)
    (cond
      ((null? namelist) #f)
      ((eq? name (car namelist)) #t)
      (else (declared? name (cdr namelist))) )))

; check if the variable is initialized or not, if it is initialized return the value, else return error
(define initialized?
  (lambda (name namelist valuelist)
    (cond
      ((null? namelist) (error "variable is not declared"))
      ((eq? name (car namelist))
            (cond
              ((not (null? (car valuelist))) (car valuelist))
              (else (error "error: variable has not been assigned a value" ))))
      (else (initialized? name (cdr namelist) (cdr valuelist))) )))

; clear the previously assigned name and value of the variable, add the new ones later
(define clear
  (lambda (name namelist valuelist)
    (cond
      ((null? namelist) '(()()))
      ((eq? name (car namelist)) (list (car (clear name (cdr namelist) (cdr valuelist)))
                                       (cadr (clear name (cdr namelist) (cdr valuelist))) ))
      (else (list (cons (car namelist) (car (clear name (cdr namelist) (cdr valuelist))))
                  (cons (car valuelist) (cadr (clear name (cdr namelist) (cdr valuelist)))))) )))

; add the name and value to state, all conditions are checked before adding
(define add
  (lambda (name value namelist valuelist)
    (cons (cons name namelist) (cons (cons value valuelist) '())) ))

; atom?
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x))) ))