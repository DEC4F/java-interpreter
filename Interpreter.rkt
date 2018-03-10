; Group 3
; Member: Shihong Ling, Stanley Tian, Yuhang Li
; Test Results: Test 1 ~ 19 work properly, I have tried Test 20 but failed T T.

; include parse tree
(load "simpleParser.scm")
(load "State.rkt")

; interpret the grammar and calculate output
(define interpret
  (lambda (file)
    (evaluate
     (call/cc
      (lambda (return)
        (M_state (parser file) (base) return (lambda (v1) (error "Error Code: INVALID_CONTINUE")) (lambda (v2) (error "Error Code: INVALID_BREAK")) (lambda (e v3) (error "Error Code: UNKNOWN_ERROR" v3)))
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
      ((list? (stmt_type statement)) (M_statement_list statement state return cont break throw))
      ;Case 1: check if statement
      ((eq? (stmt_type statement) 'if) (M_if statement state return cont break throw))
      ;Case 2: check return statement
      ((eq? (stmt_type statement) 'return) (M_return statement state return cont break throw))
      ;Case 3: check while statement
      ((eq? (stmt_type statement) 'while) (M_while (condition statement) (loop_body statement) state return cont break throw))
      ;Case 4: check declare statement
      ((eq? (stmt_type statement) 'var) (M_declare statement state return cont break throw))
      ;Case 5: check assign statement
      ((eq? (stmt_type statement) '=) (M_assign statement state return cont break throw))
      ;Case 6: check throw
      ((eq? (stmt_type statement) 'throw) (throw (thrown_part statement) state))
      ;Case 7: check break
      ((eq? (stmt_type statement) 'break) (break state))
      ;Case 8: check continue
      ((eq? (stmt_type statement) 'continue) (cont state))
      ;Case 9: check begin
      ((eq? (stmt_type statement) 'begin) (M_block statement state return cont break throw))
      ;Case 10: check tey catch finally
      ((eq? (stmt_type statement) 'try) (M_try statement state return cont break throw))
      ;Case 11: else situation
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
      ((M_value (condition statement) state) (M_state (then statement) state return cont break throw))
      ((hasElse? statement) (M_state (else_branch statement) state return cont break throw))
      (else state))))

;interprets return statement
(define M_return
  (lambda (statement state return cont break throw)
    (return (M_value (variable statement) state))))

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
      ;Case 3: the value is either a variable or number
      ((null? (second expression)) (M_value (first statement) state))
      ;Case 4: the value is the result of +
      ((eq? (operator expression) '+) (+ (M_value (operand_1 expression) state) (M_value (operand_2 expression) state)))
      ;Case 5: the value is the result of -
      ((eq? (operator expression) '-) (cond
                                   ((eq? (second_operand_part expression) '()) (- 0 (M_value (operand_1 expression) state)))
                                   (else (- (M_value (operand_1 expression) state) (M_value (operand_2 expression) state))) ))
      ;Case 6: the value is the result of *
      ((eq? (operator expression) '*) (* (M_value (operand_1 expression) state) (M_value (operand_2 expression) state)))
      ;Case 7: the value is the result of quotient
      ((eq? (operator expression) '/) (quotient (M_value (operand_1 expression) state) (M_value (operand_2 expression) state)))
      ;Case 8: the value is the result of remainder
      ((eq? (operator expression) '%) (remainder (M_value (operand_1 expression) state) (M_value (operand_2 expression) state)))    
      ;Case 9: expression is the result of ==
      ((eq? (operator expression) '==) (eq? (M_value (operand_1 expression) state) (M_value (operand_2 expression) state)))
      ;Case 10: expression is the result of >
      ((eq? (operator expression) '>) (> (M_value (operand_1 expression) state) (M_value (operand_2 expression) state)))
      ;Case 11: expression is the result of <
      ((eq? (operator expression) '<) (< (M_value (operand_1 expression) state) (M_value (operand_2 expression) state)))
      ;Case 12: expression is the result of >=
      ((eq? (operator expression) '>=) (>= (M_value (operand_1 expression) state) (M_value (operand_2 expression) state)))
      ;Case 13: expression is the reult of <=
      ((eq? (operator expression) '<=) (<= (M_value (operand_1 expression) state) (M_value (operand_2 expression) state)))
      ;Case 14: expression is the result of !=
      ((eq? (operator expression) '!=) (not (= (M_value (operand_1 expression) state) (M_value (operand_2 expression) state))))
      ;Case 15: expression is the result of ||
      ((eq? (operator expression) '||) (or (M_value (operand_1 expression) state) (M_value (operand_2 expression) state)))
      ;Case 16: expression is the result of &&
      ((eq? (operator expression) '&&) (and (M_value (operand_1 expression) state) (M_value (operand_2 expression) state)))
      ;Case 17: expression is the result of !
      ((eq? (operator expression) '!) (not (M_value (operand_1 expression) state)))
      ;Case error: check declare or initialization error
      ((or (eq? 'uninitialized (M_value (operand_1 expression) state)) (eq? 'undeclared (M_value (operand_1 expression) state))) (error "Error Code: UNDEFINED_OR_UNINITIALIZED_VARIABLE"))
      ((and (not (null? (second_operand_part expression))) (or (eq? 'uninitialized (M_value (operand_2 expression) state)) (eq? 'undeclared (M_value (operand_2 expression) state)))) (error "Error Code: UNDEFINED_OR_UNINITIALIZED_VARIABLE"))
      )))

; Declare variable
(define M_declare
  (lambda (statement state return cont break throw)
    (cond
      ; edge case: var already declared
      ((inState? (variable statement) state) (error "Error Code: REDEFINING_VARIABLE"))
      ; case 1: declare a var but not assign any value
      ((null? (declare_right_side statement)) (add_to_state (variable statement) 'uninitialized state))
      ; case 2: declare a var and assign a value to it
      (else (add_to_state (variable statement) (M_value (value statement) state) (M_state (value statement) state return cont break throw))) 
       )))

; Assign value
(define M_assign
  (lambda (statement state return cont break throw)
    (if (inState? (variable statement) state)
        (add_to_state (variable statement) (M_value (value statement) state) state)
        (error "Error Code: USING_BEFORE_DECLARING") )))

; Evaluate the while statement returns state
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

; Process a block of code
(define M_block
  (lambda (statement state return cont break throw)
    (pop_layer (M_statement_list (rest statement) (push_layer state) return (lambda (w) (cont (pop_layer w))) (lambda (w) (break (pop_layer w))) (lambda (w s) (throw w (pop_layer s)))))))

; Try-Catch-Finally
(define M_try
  (lambda (statement state return cont break throw)
    (cond
      ((not (hascatch? statement)) (M_state (finallybody statement) (M_state (trybody statement) state return cont break throw) return cont break throw))
      ((not (hasfinally? statement))
       (call/cc
        (lambda (new_throw)
          (M_state (trybody statement) state return cont break (lambda (e new_state) (new_throw (M_catch (catchbody statement) e (errorName statement) new_state return cont break throw))))
          )))
      (else (M_state (finallybody statement)
                     (call/cc
                      (lambda (new_throw)
                        (M_state (trybody statement) state return cont break
                                 (lambda (e new_state) (new_throw (M_catch (catchbody statement) e (errorName statement) new_state return cont break throw))))))
                     return break cont throw)))))

; Code for catch part
(define M_catch
  (lambda (statement error errorName state return cont break throw)
      (M_state statement (add_to_state errorName error state) return cont break throw)))
    

; ------------------------------------------------HELPERS-----------------------------------------------------------
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

; Check whether the try is followed by catch
(define hascatch?
  (lambda (statement)
    (not (null? (caddr statement)))))

;Check whether the try has finally
(define hasfinally?
  (lambda (statement)
    (not (null? (cadddr statement)))))

;Check whether the catch contain throw
(define find_num
  (lambda (x list)
    (cond
      ((null? list) 0)
      ((list? (car list)) (+ (find_num x (car list)) (find_num x (cdr list))))
      ((eq? (car list)) (+ 1 (find_num x (cdr list))))
      (else (find_num x (cdr list))))))
; -----------------------------------------ABSTRACTION HELPERS-------------------------------------------------------
(define thrown_part cadr)
(define operand_1 cadr)
(define operand_2 caddr)
(define condition cadr)
(define then caddr)
(define else_branch cadddr)
(define loop_body caddr)
(define operator car)
(define stmt_type car)
(define second_operand_part cddr)
(define declare_right_side cddr)
(define variable cadr)
(define value caddr)
(define trybody cadr)
(define catchbody
  (lambda (statement)
    (cdr (cdaddr statement))))
(define finallybody
  (lambda (statement)
    (cadr (cadddr statement))))
(define errorName
  (lambda (statement)
    (caar (cdaddr statement))))