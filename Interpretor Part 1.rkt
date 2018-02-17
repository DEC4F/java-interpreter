; Group 69
; Member: Shihong Ling, Yuhang Li, Xiangda Tian

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
         ((M_boolean (car (cdr statement)) state) (M_state (car (cddr statement)) state))
         ((null? (cdr (cdr (cdr statement)))) state)
         (else (M_state (car (cdddr statement)) state))))
      ;check return statement
      ((eq? (car statement) 'return)
       (cond
         ((not (isBooleanOperation (car (cdr statement)) (car state) (car (cdr state)))) (M_value (car (cdr statement)) state))
         ((M_boolean (car (cdr statement)) state) 'true)
         (else 'false) ))
      ;check while statement
      ((eq? (car statement) 'while) (M_while (car (cdr statement)) (car (cddr statement)) state))

      ;need assign declare

      )))
      

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
      ((atom? condition) (find condition (car state) (car (cdr state))))
      ((eq? (car condition) '==) (= (M_value (car (cdr condition)) state) (M_value (car (cddr condition)) state)))
      ((eq? (car condition) '>) (> (M_value (car (cdr condition)) state) (M_value (car (cddr condition)) state)))
      ((eq? (car condition) '<) (< (M_value (car (cdr condition)) state) (M_value (car (cddr condition)) state)))
      ((eq? (car condition) '>=) (or (= (M_value (car (cdr condition)) state) (M_value (car (cddr condition)) state))
                                     (> (M_value (car (cdr condition)) state) (M_value (car (cddr condition)) state))))
      ((eq? (car condition) '<=) (or (= (M_value (car (cdr condition)) state) (M_value (car (cddr condition)) state))
                                     (< (M_value (car (cdr condition)) state) (M_value (car (cddr condition)) state))))
      ((eq? (car condition) '!=) (not (= (M_value (car (cdr condition)) state) (M_value (car (cddr condition)) state))))
      ((eq? (car condition) '||) (or (M_boolean (car (cdr condition)) state) (M_boolean (car (cddr condition)) state)))
      ((eq? (car condition) '&&) (and (M_boolean (car (cdr condition)) state) (M_boolean (car (cddr condition)) state)))
      ((eq? (car condition) '!) (not (M_boolean (cdr condition) state))) )))

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

; calculate the arithmetic expressions and return the number after calculation
(define M_value
  (lambda (expression state)
    (cond
      ((null? expression) ())
      ((number? expression) expression)
      ((atom? expression) (find expression (car state) (car (cdr state))))
      ((eq? (car expression) '+) (+ (M_value (car (cdr expression)) state) (M_value (car (cdr (cdr expression))) state)))
      ((eq? (car expression) '-) (cond
                                   ((eq? (cdr (cdr expression)) '()) (- 0 (M_value (car (cdr expression)) state)))
                                   (else (- (M_value (car (cdr expression)) state) (M_value (car (cdr (cdr expression))) state))) ))
      ((eq? (car expression) '*) (* (M_value (car (cdr expression)) state) (M_value (car (cdr (cdr expression))) state)))
      ((eq? (car expression) '/) (/ (M_value (car (cdr expression)) state) (M_value (car (cdr (cdr expression))) state)))
      ((eq? (car expression) '%) (remainder (M_value (car (cdr expression)) state) (M_value (car (cddr expression)) state)))
      ((number? (car expression)) (car expression))
      ((atom? (car expression)) (find (car expression) (car state) (car (cdr state))))
      ((list? (car expression)) (M_value (car expression) state)) )))

; find the value by given name if not declared or not initialized return error
(define find
  (lambda (name namelist valuelist)
    (cond
      ((declared? name namelist) (initialized? name namelist valuelist))
      (else (error "error: you need to declare first")) )))

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
                                       (car (cdr (clear name (cdr namelist) (cdr valuelist)))) ))
      (else (list (cons (car namelist) (car (clear name (cdr namelist) (cdr valuelist))))
                  (cons (car valuelist) (car (cdr (clear name (cdr namelist) (cdr valuelist))))))) )))

; add the name and value to state, all conditions are checked before adding
(define add
  (lambda (name value namelist valuelist)
    (cons (cons name namelist) (cons (cons value valuelist) '())) ))

; atom?
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x))) ))

; isaojdasjdsadjkgk