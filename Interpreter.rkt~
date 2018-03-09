; Group 3
; Member: Shihong Ling, Yuhang Li, Stanley Tian

; include parse tree
(load "simpleParser.scm")
(load "State.rkt")

; interpret the grammar and calculate output
(define interpret
  (lambda (file)
    (evaluate
     (call/cc
      (lambda (return)
        (M_state (parser file) (base) return (lambda (v1) (error "Error Code: INVALID_CONTINUE")) (lambda (v2) (error "Error Code: INVALID_BREAK")) (lambda (v3) (error: "Error Code: UNKNOWN_ERROR" v3)))
        )))))

; evaluate the statemet in the parse tree if every statement is evaluated, return the state which will store the output
(define evaluate
  (lambda (x)
    (cond
      ((eq? x #t) 'true)
      ((eq? x #f) 'false)
      ((eq? x 'undeclared) (error "Error Code: USING_BEFORE_DECLARING"))
      ((eq? x 'uninitialized) (error "Error Code: USING_BEFORE_ASSIGNING"))
      (else x))))

; check each type of statement and update the state
(define M_state
  (lambda (statement state return cont break throw)
    (cond
      ;Edge case 1: empty statement
      ((null? statement) state)
      ;Edge case 2: statement is an atom
      ((isAtom statement) state)
      ;Case 0; check list of statements
      ((list? (first statement)) (M_statement_list statement state return cont break throw))
      ;Case 1: check if statement
      ((eq? (first statement) 'if) (M_if statement state return cont break throw))
      ;Case 2: check return statement
      ((eq? (first statement) 'return) (M_return statement state return cont break throw))
      ;Case 3: check while statement
      ((eq? (first statement) 'while) (M_while (second statement) (third statement) state return cont break throw))
      ;Case 4: check declare statement
      ((eq? (first statement) 'var) (M_declare statement state return cont break throw))
      ;Case 5: check assign statement
      ((eq? (first statement) '=) (M_assign statement state return cont break throw))
      ;Case 6: check throw
      ((eq? 'throw (first statement)) (throw (second statement) state))
      ;Case 7: check break
      ((eq? 'break (first statement)) (break state))
      ;Case 8: check continue
      ((eq? 'continue (first statement)) (cont state))
      ;Case 9: check begin
      ((eq? 'begin (first statement)) (M_block statement state return cont break throw))
      (else state)
      )))
      
;interprets list of statement
(define M_statement_list
  (lambda (statements state return cont break throw)
    (if (null? statements) 
      state
      (M_statement_list (rest statements) (M_state (first statements) state return cont break throw) return cont break throw))))

;interprets if statement
(define M_if
  (lambda (statement state return cont break throw)
    (cond
      ((isDeclared statement state) (error 'variableUndeclared "Error Code: USING_BEFORE_DECLARING"))
      ((isInit statement state) (error 'variableUninitialized "Error Code: USING_BEFORE_ASSIGNING"))
      ((M_value (second statement) state) (M_state (third statement) state return cont break throw))
      ((hasElse? statement) (M_state (else_branch statement) state return cont break throw))
      (else state))))

;interprets return statement
(define M_return
  (lambda (statement state return cont break throw)
    (return (M_value (second statement) state))))

; calculate the arithmetic expressions and return the number after calculation
(define M_value
  (lambda (expression state)
    (cond
      ;Base Case: no expression, return empty
      ((null? expression) '())
      ;Case 1: the value is the number
      ((number? expression) expression)
      ;Case 2: the value is the value of the variable
      ((isAtom expression) (lookup_list expression state))
      ;Case 3:
      ((null? (second expression)) (M_value (first statement) state))

      ;not sure
      ((or (eq? 'uninitialized (M_value (second expression) state)) (eq? 'undeclared (M_value (second expression) state))) (error "Error Code: UNDEFINED_OR_UNINITIALIZED_VARIABLE"))
      ((and (not (null? (rest_after_two expression))) (or (eq? 'uninitialized (M_value (third expression) state)) (eq? 'undeclared (M_value (third expression) state)))) (error "Error Code: UNDEFINED_OR_UNINITIALIZED_VARIABLE"))
      
      ;Case 4: the value is the result of +
      ((eq? (operator expression) '+) (+ (M_value (second expression) state) (M_value (third expression) state)))
      ;Case 5: the value is the result of -
      ((eq? (operator expression) '-) (cond
                                   ((eq? (rest_after_two expression) '()) (- 0 (M_value (second expression) state)))
                                   (else (- (M_value (second expression) state) (M_value (third expression) state))) ))
      ;Case 6: the value is the result of *
      ((eq? (operator expression) '*) (* (M_value (second expression) state) (M_value (third expression) state)))
      ;Case 7: the value is the result of quotient
      ((eq? (operator expression) '/) (quotient (M_value (second expression) state) (M_value (third expression) state)))
      ;Case 8: the value is the result of remainder
      ((eq? (operator expression) '%) (remainder (M_value (second expression) state) (M_value (third expression) state)))
      ;Case 9: this is for when we have "=" in a expression
      ;((eq? (operator expression) '=) (M_value (second expression) (M_state expression state)) )    
      ;Case 10: expression is the result of ==
      ((eq? (operator expression) '==) (eq? (M_value (second expression) state) (M_value (third expression) state)))
      ;Case 11: expression is the result of >
      ((eq? (operator expression) '>) (> (M_value (second expression) state) (M_value (third expression) state)))
      ;Case 12: expression is the result of <
      ((eq? (operator expression) '<) (< (M_value (second expression) state) (M_value (third expression) state)))
      ;Case 13: expression is the result of >=
      ((eq? (operator expression) '>=) (>= (M_value (second expression) state) (M_value (third expression) state)))
      ;Case 14: expression is the reult of <=
      ((eq? (operator expression) '<=) (<= (M_value (second expression) state) (M_value (third expression) state)))
      ;Case 15: expression is the result of !=
      ((eq? (operator expression) '!=) (not (= (M_value (second expression) state) (M_value (third expression) state))))
      ;Case 16: expression is the result of ||
      ((eq? (operator expression) '||) (or (M_value (second expression) state) (M_value (third expression) state)))
      ;Case 17: expression is the result of &&
      ((eq? (operator expression) '&&) (and (M_value (second expression) state) (M_value (third expression) state)))
      ;Case 28: expression is the result of !
      ((eq? (operator expression) '!) (not (M_value (second expression) state)))
      )))

; Declare variable
(define M_declare
  (lambda (statement state return cont break throw)
    (cond
      ; edge case: var already declared
      ((inState? (second statement) state) (error "Error Code: REDEFINING_VARIABLE"))
      ; case 1: declare a var but not assign any value
      ((null? (rest_after_two statement)) (add_to_state (second statement) 'uninitialized state))
      ; case 2: declare a var and assign a value to it
      (else (add_to_state (second statement) (M_value (third statement) state) (M_state (third statement) state return cont break throw))) 
       )))

; Assign value
(define M_assign
  (lambda (statement state return cont break throw)
    (if (inState? (second statement) state)
        (add_to_state (second statement) (M_value (third statement) state) state)
        (error "Error Code: USING_BEFORE_DECLARING") )))

; evaluate the while statement returns state
(define M_while
  (lambda (predicate body state return cont break throw)
    (call/cc
      (lambda (break2)
        (letrec ((while_loop (lambda (predicate body state return cont break throw)
          (if (M_value predicate state)
            (while_loop predicate body
              (call/cc 
                (lambda (cont2) 
                  (M_state body state return cont2 break throw)))
              return cont break throw)
            state))))
          (while_loop predicate body state return cont break2 throw) )))))

; process a block of code
(define M_block
  (lambda (statement state return cont break throw)
    (pop_layer (M_statement_list (rest statement) (push_layer state) return (lambda (w) (cont (pop_layer w))) (lambda (w) (break (pop_layer w))) (lambda (w s) (throw w (pop_layer s)))))))

; ---------------------------------------HELPERS--------------------------------
; Check if the name is declared
(define isDeclared
  (lambda (statement state)
    (eq? 'undeclared (M_value (second statement) state))
     ))

; isAtom, check whether the element is an atom
(define isAtom
  (lambda (x)
    (and (not (pair? x)) (not (null? x))) ))

; Checks to see if a variable is already in the state
(define inState?
  (lambda (name state)
    (not (eq? 'undeclared (lookup_list name state))) ))

; Checks to see if the statement is in the environment 
(define isInit
  (lambda (statement state)
    (eq? 'uninitialized (M_value (second statement) state)) ))

; Check else part in if statement
(define hasElse?
  (lambda (statement)
    (not (null? (cdddr statement))) ))

; -----------------------------------------ABSTRACTION HELPERS-------------------------------------------------------
(define condition car)
(define else_branch cadddr)
(define operator car)
(define rest_after_two cddr)

(define break cadr)
(define cont caddr)
(define throw cadddr)