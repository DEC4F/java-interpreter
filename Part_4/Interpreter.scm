;Group Number: 4
;Group Member: Shihong Ling, Mingxuan Ju, Stanley Tian
(load "classParser.scm")
(load "Environment.scm")
(load "Class.scm")
;-----------------
; Main Interpret Part
;-----------------
; Interpret input program file and return result
(define interpret
  (lambda (file class)
    (scheme->language
     (let*
         ((env (interpret-outer (parser file) (newenv) (default-collection))))
     (call/cc
      (lambda (return)
        (interpret-value (list 'funcall (list 'dot class 'main)) env (set-return return (default-collection)))))
       ))))

; Interpret outer scope of the program
(define interpret-outer
  (lambda (block env collection)
    (cond
      ((null? block) env)
      ((eq? (car (car block)) 'class) (interpret-outer (cdr block) (interpret-class (car block) env collection) collection))
      (else (myerror "scope is not valie for the class: " block)))))

;-----------------
; Default Collection (Return + Break + Cont + Throw + Class + Instance + Current Class)
;-----------------

; These helper methods define each part of collection
(define return-in-c car)
(define break-in-c cadr)
(define cont-in-c caddr)
(define throw-in-c cadddr)
(define class-in-c (lambda (l) (list-ref l 4)))
(define instance-in-c (lambda (l) (list-ref l 5)))
(define current-in-c (lambda (l) (list-ref l 6)))

; Default value for return
(define default-return
  (lambda (v)
    (myerror "error: return used outside of the function call")))

; Default value for break
(define default-break
  (lambda (env)
    (myerror "error: break used outside of loop")))

; Default value for continue
(define default-continue
  (lambda (env)
    (myerror "error: continue used outside of loop")))

; Default value for throw 
(define default-throw
  (lambda (v env)
    (myerror "error: uncaught exception thrown")))

; Default value for collection
(define default-collection
  (lambda ()
    (list default-return default-break default-continue default-throw 'null 'null 'null)))

; Set up return
(define set-return
  (lambda (return c)
    (replace 0 return c)))

; Set up break
(define set-break
  (lambda (break c)
    (replace 1 break c)))

; Set up continue
(define set-cont
  (lambda (cont c)
    (replace 2 cont c)))

; Set up throw 
(define set-throw
  (lambda (throw c)
    (replace 3 throw c)))

; Set up class
(define set-class
  (lambda (class c)
    (replace 4 class c)))

; Set up instance
(define set-instance
  (lambda (instance c)
    (replace 5 instance c)))

; Set up current instance
(define set-current
  (lambda (currclass c)
    (replace 6 currclass c)))

;-----------------
; Abstractions
;-----------------
; These helper functions creates abstraction on conditions in finding M value of a statement
(define negate? (lambda (statement) (eq? '! (operator statement))))
(define negnum? (lambda (statement) (and (eq? '- (operator statement)) (= 2 (length statement)))))
(define addition? (lambda (statement) (eq? '+ (operator statement))))
(define subtraction? (lambda (statement) (eq? '- (operator statement))))
(define multiply? (lambda (statement) (eq? '* (operator statement))))
(define division? (lambda (statement) (eq? '/ (operator statement))))
(define mod? (lambda (statement) (eq? '% (operator statement))))
(define compare? (lambda (statement) (eq? '== (operator statement))))
(define neq? (lambda (statement) (eq? '!= (operator statement))))
(define less? (lambda (statement) (eq? '< (operator statement))))
(define greater? (lambda (statement) (eq? '> (operator statement))))
(define leq? (lambda (statement) (eq? '<= (operator statement))))
(define geq? (lambda (statement) (eq? '>= (operator statement))))
(define or-stmt? (lambda (statement) (eq? '|| (operator statement))))
(define and-stmt? (lambda (statement) (eq? '&& (operator statement))))

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

(define statement-list?
  (lambda (statement)
    (list? (car statement)) ))

; These helper functions define the parts of the various statement types
(define vartype car)
(define expr caddr)
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)
(define binding operand1)

; Check whether the try is followed by catch
(define hascatch?
  (lambda (statement)
    (not (null? (caddr statement)))))

;Check whether the try has finally
(define hasfinally?
  (lambda (statement)
    (not (null? (cadddr statement)))))

; These helper methods defines parts of try-catch-finally
(define trybody cadr)
(define catchbody
  (lambda (statement)
    (cdr (cdaddr statement))))
(define finallybody
  (lambda (statement)
    (cadr (cadddr statement))))
(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))
(define errorName
  (lambda (statement)
    (caar (cdaddr statement))))

; These hellper functions define the parts of a defined function
(define function-name cadr)
(define function-body cadr)
(define function-env caddr)
(define get-parameter caddr)
(define get-funcbody cadddr)
(define formal-parameter car)
(define actual-parameter cddr)
(define first-para car)
(define rest-para cdr)

;-----------------
; interpret-value part
;-----------------
; Interpret statement and get the value of it
(define interpret-value
  (lambda (statement env collection)
    (cond
      ((and (list? statement) (eq? '= (operator statement))) (M-value-assign statement env collection))
      ((and (list? statement) (eq? 'funcall (operator statement))) (M-value-funcall statement env collection))
      ((and (list? statement) (eq? 'dot (operator statement))) (M-value-dot statement env collection))
      ((and (list? statement) (eq? 'new (operator statement))) (M-value-new statement env collection))
      ((list? statement) (M-value-expression statement env collection))
      (else (M-value-atom statement env collection))  )))

; Get the value of expression
(define M-value-expression
  (lambda (statement env collection)
    (cond
      ((negate? statement) (not (interpret-value (operand1 statement) env collection)))
      ((negnum? statement) (- 0 (interpret-value (operand1 statement) env collection)))
      ((addition? statement) (+ (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      ((subtraction? statement) (- (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      ((multiply? statement) (* (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      ((division? statement) (quotient (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      ((mod? statement) (remainder (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      ((compare? statement) (== (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      ((neq? statement) (!= (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      ((less? statement) (< (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      ((greater? statement) (> (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      ((leq? statement) (<= (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      ((geq? statement) (>= (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      ((or-stmt? statement) (or (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      ((and-stmt? statement) (and (interpret-value (operand1 statement) env collection) (interpret-value (operand2 statement) env collection)))
      (else (myerror "error: unknown operator:" (operator statement))))))


; Get the value of "new" object
(define M-value-new
  (lambda (statement env collection)
    (let ((class (check-binding (binding statement) (get-base-frame env))))
      (cond
        ((not (and (list? class) (eq? (car class) 'class))) (myerror "error: invalid class: " (cadr statement)))
        (else (set-instant-value (newinstance class) (box-list (binding (instance-of-class class)))))))))

; Helper methods for M-value-new
(define get-base-frame
  (lambda (env)
    (if (null? (cdr env))
        env
        (get-base-frame (cdr env)))))
(define box-list
  (lambda (l)
    (map box l)))

; Get the value of an atom
(define M-value-atom
  (lambda (statement env collection)
    (cond
      ((or (boolean? statement) (number? statement)) statement)
      ((eq? statement 'true) #t)
      ((eq? statement 'false) #f)
      ((eq? 'undefined (M-value-var statement env collection)) (myerror "Undefined variable: " statement))
      (else (M-value-var statement env collection)) )))

; Get the value of an assignment (ex: return "a = 1")
(define M-value-assign
  (lambda (statement env collection)
    (let* ((var (find-var (get-assign-lhs statement) env collection))
           (value (interpret-value (get-assign-rhs statement) env collection)) )
      (set-box! var value)
      value)))

; Get the value of an variable in environment
(define M-value-var
  (lambda (statement env collection)
    (let ((box (find-var-box statement env (current-in-c collection) (instance-in-c collection))))
      (cond
        ((eq? box 'no_value) (myerror "error: can not find variable: " statement))
        (else (unbox box)) ))))

; Get the value of a "dot" object
(define M-value-dot
  (lambda (statement env collection)
    (unbox (find-dot-var statement env collection)) ))

; Get the value of a function call
(define M-value-funcall
  (lambda (f env collection)
    (let* ((l (find-func (function-name f) env collection))
           (closure (car l))
           (instance (cadr l))
           (instance ((list-ref closure 4) instance))
           (class (caddr l))
           (currclass ((cadddr closure) env))
           (class (if (eq? 'null instance) currclass class))
           (outerenv env)
           (newenv (cons (new-frame-parameter (formal-parameter closure) (actual-parameter f) env collection) outerenv))
           (error (lambda (v) (myerror "error: illegal break or continue"))) )
      (call/cc
       (lambda (return)
         (interpret-statement-list (function-body closure) newenv (set-class class (set-return return (set-break error (set-instance instance (set-current currclass (set-cont error collection))))))
                                   ))))))
; Helper method for M-value-funcall
(define new-frame-parameter
  (lambda (formal actual env collection)
    (cond
      ((and (null? formal) (null? actual)) (newframe))
      ((or (null? formal) (null? actual)) (myerror "error: arguments number error"))
      ((eq? '& (first-para formal)) (add-to-frame (cadr formal)
                                                  (find-var (first-para actual) env colleaction)
                                                  (new-frame-parameter (actual-parameter formal) (rest-para actual) env collection) ))
      (else (add-to-frame (first-para formal)
                          (box (interpret-value(first-para actual) env collection))
                          (new-frame-parameter (rest-para formal) (rest-para actual) env collection) )) )))

      
;-----------------
; interpret-statement part
;-----------------
; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list env collection)
    (if (null? statement-list)
        env
        (interpret-statement-list (cdr statement-list)
                                      (interpret-statement (car statement-list) env collection)
                                  collection))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement env collection)
    (cond
      ((null? statement) env)
      ((list? statement) (cond
                           ((statement-list? statement) (interpret-statement-list statement env collection))
                           ((eq? 'function (statement-type statement)) (interpret-function statement env collection))
                           ((eq? 'funcall (statement-type statement)) (interpret-funcall statement env collection))
                           ((eq? 'return (statement-type statement)) (interpret-return statement env collection))
                           ((eq? 'var (statement-type statement)) (interpret-declare statement env collection))
                           ((eq? '= (statement-type statement)) (interpret-assign statement env collection))
                           ((eq? 'if (statement-type statement)) (interpret-if statement env collection))
                           ((eq? 'while (statement-type statement)) (interpret-while statement env collection))
                           ((eq? 'continue (statement-type statement)) ((cont-in-c collection) env))
                           ((eq? 'break (statement-type statement)) ((break-in-c collection) env))
                           ((eq? 'begin (statement-type statement)) (interpret-block statement env collection))
                           ((eq? 'try (statement-type statement)) (interpret-try statement env collection))
                           ((eq? 'throw (statement-type statement)) (interpret-throw statement env collection))
                           (else env)))
      (else env))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement env collection)
    ((return-in-c collection) (interpret-value (get-expr statement) env collection))))

; Defines functions
(define interpret-function
  (lambda (f env collection)
    (let ((fname (function-name f)))
      (add-to-env fname (list (get-parameter f)
                          (get-funcbody f)
                          (lambda (env) (get-env f env))
                          (lambda (env) (class-in-c collection))
                          (lambda (v) v)) env) )))


; Calls functions
(define interpret-funcall
  (lambda (statement env collection)
    (begin (interpret-value statement env collection) env)))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement env collection)
    (cond
      ((= (length statement) 3) (add-to-env (get-declare-var statement) (interpret-value (get-declare-value statement) env collection) (interpret-statement (get-declare-value statement) env collection))) 
      (else (add-to-env (get-declare-var statement) 'undefined env) ))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement env collection)
    (begin (M-value-assign statement env collection) env) ))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement env collection)
    (cond
      ((interpret-value (get-condition statement) env collection) (interpret-statement (get-then statement) env collection))
      ((exists-else? statement) (interpret-statement (get-else statement) env collection))
      (else env))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement env collection)
    (call/cc
     (lambda (new-break)
       (letrec
           ((loop (lambda (condition body env)
                        (cond
                          ((interpret-value condition env (set-break new-break collection))
                            (loop condition body
                                  (call/cc (lambda (new-continue)
                                                            (interpret-statement body env (set-cont new-continue (set-break new-break collection)))))))
                          (else env) ))))
         (loop (get-condition statement) (get-body statement) env) )))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement env collection)
    (removeframe
     (interpret-statement-list (cdr statement)
                               (addframe env)
                               (set-break (lambda (v) ((break-in-c collection) (removeframe v))) (set-cont (lambda (v) ((cont-in-c collection) (removeframe v))) collection) ) ))))
                                         

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement env collection)
    ((throw-in-c collection) (interpret-value (get-expr statement) env collection) env)))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
; We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment collection)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment collection) (return v))))
              (new-break (lambda (env) ((break-in-c collection) (interpret-block finally-block env collection))))
              (new-continue (lambda (env) ((cont-in-c collection) (interpret-block finally-block env collection))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment collection jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment (set-return new-return (set-break new-break (set-cont new-continue (set-throw new-throw collection)))))
                          collection))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))
(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
; Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment collection jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env collection)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (removeframe (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (add-to-env (catch-var catch-statement) ex (addframe env))
                                                 (set-break (lambda (env2) ((break-in-c collection) (removeframe env2))) (set-cont (lambda (env2) ((cont-in-c collection) (removeframe env2))) (set-throw (lambda (v env2) ((throw-in-c collection) v (removeframe env2))) collection)))
                                                 ))
                                     collection)))))))


;------------------------
; Usefule helper method
;------------------------
; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (call/cc
     (lambda (break)
       (indexof-h var l break)))))
(define indexof-h
  (lambda (var l break)
    (cond
      ((null? l) (break -1))  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof-h var (cdr l) break))))))


; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Check whether input is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x))) ))

; Checks to see if two values are equal
(define ==
  (lambda (x y)
    (cond
     ((and (number? x) (number? y)) (= x y))
     ((and (atom? x) (atom? y)) (eqv? x y))
     ((and (list? x) (list? y)) (equal? x y))
     (else #f) )))

; Checks to see if two values are NOT equal 
(define !=
  (lambda (x y)
    (not (== x y)) ))

; Replace value in a list at specifix index
(define replace
  (lambda (index value list)
    (cond
      ((= 0 index) (cons value (cdr list)))
      (else (cons (car list) (replace (- index 1) value (cdr list)))))))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.
(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))
