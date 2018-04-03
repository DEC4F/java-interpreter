;Group Number: 3
;Group Member: Shihong Ling, Mingxuan Ju, Stanley Tian
;Reference: our interpret3 is based on the interpret2-callcc-no-boxes.scm provided by Prof.Connamacher
(load "functionParser.scm")


; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
;(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file)
    (scheme->language
     (let*
         ((env (interpret-statement (parser file) (newenvironment) default-return default-break default-continue default-throw)))
     (call/cc
      (lambda (return)
        (interpret-value '(funcall main) env return default-break default-continue default-throw)))
       ))))

;------------------------Default Errors-----------------------------------------------
(define default-return
  (lambda (v)
    (myerror "Return used outside of the function call")))
    
(define default-break
  (lambda (env)
    (myerror "Break used outside of loop")))

(define default-continue
  (lambda (env)
    (myerror "Continue used outside of loop")))

(define default-throw
  (lambda (v env)
    (myerror "Uncaught exception thrown")))
;--------------------------------------------------------------------------------------  

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list)
                                      (interpret-statement (car statement-list) environment return break continue throw)
                                  return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((null? statement) environment)
      ((not (list? statement)) environment)
      ((statement-list? statement) (interpret-statement-list statement environment return break continue throw))
      ((eq? 'function (statement-type statement)) (interpret-function statement environment return break continue throw))
      ((eq? 'funcall (statement-type statement)) (interpret-funcall statement environment return break continue throw))
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return break continue throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment return break continue throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment return break continue throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return break continue throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return break continue throw)
    (return (interpret-value (get-expr statement) environment return break continue throw))))

; Defines functions
(define interpret-function
  (lambda (statement environment return break continue throw)
    (insert (function-name statement)
                  (list (get-parameter statement) (get-funcbody statement)
                        (lambda (environment)
                          (get-function-frame (function-name statement) environment))) environment)))
; Calls functions
(define interpret-funcall
  (lambda (statement environment return break continue throw)
    (interpret-value statement environment return break continue throw) environment))

; Return the state after we call a function. Mainly used to deal with global variables.
(define interpret-funstate
  (lambda (statement environment return break continue throw)
    (let* ((closure (lookup (function-name statement) environment))
           (new-environment (cons (new-frame-parameter (formal-parameter closure) (actual-parameter statement) environment return break continue throw) environment)))
         (interpret-statement-list (removereturn (function-body closure)) new-environment return default-break default-continue throw))))
    

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment return break continue throw)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (interpret-value (get-declare-value statement) environment return break continue throw)
                (interpret-statement (get-declare-value statement) environment return break continue throw))
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment return break continue throw)
    (update (get-assign-lhs statement) (interpret-value (get-assign-rhs statement) environment return break continue throw) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((interpret-value (get-condition statement) environment return break continue throw) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (new-break)
       (letrec ((loop (lambda (condition body environment)
                        (if (interpret-value condition environment return break continue throw)
                            (loop condition body (call/cc (lambda (new-continue)
                                                            (interpret-statement body environment return new-break new-continue throw))))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

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

; Evaluates all possible boolean and arithmetic expressions, including constants and variables. 
(define interpret-value
  (lambda (statement environment return break continue throw)
    (cond
      ((null? statement) '())
      ((number? statement) statement)
      ((eq? statement 'true) #t)
      ((eq? statement 'false) #f)
      ((not (list? statement)) (lookup-val statement environment))
      ((eq? 'funcall (operator statement)) (eval-funcall statement environment return break continue throw))
      ((eq? '! (operator statement)) (not (interpret-value (operand1 statement) environment return break continue throw)))
      ((and (eq? '- (operator statement)) (= 2 (length statement))) (- (interpret-value (operand1 expr) environment return break continue throw)))
      ((eq? '+ (operator statement)) (+ (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw)))
      ((eq? '- (operator statement)) (- (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw)))
      ((eq? '* (operator statement)) (* (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw)))
      ((eq? '/ (operator statement)) (quotient (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw)))
      ((eq? '% (operator statement)) (remainder (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw)))
      ((eq? '== (operator statement)) (= (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw)))
      ((eq? '!= (operator statement)) (not (= (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw))))
      ((eq? '< (operator statement)) (< (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw)))
      ((eq? '> (operator statement)) (> (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw)))
      ((eq? '<= (operator statement)) (<= (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw)))
      ((eq? '>= (operator statement)) (>= (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw)))
      ((eq? '|| (operator statement)) (or (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw)))
      ((eq? '&& (operator statement)) (and (interpret-value (operand1 statement) environment return break continue throw) (interpret-value (operand2 statement) environment return break continue throw)))
      (else (myerror "Unknown operator:" (operator expr))))))


; Evaluate functions when the functions are called
(define eval-funcall
  (lambda (statement environment return break continue throw)
    (let* ((closure (lookup-val (function-name statement) environment))
           (new-environment (cons (new-frame-parameter (formal-parameter closure) (actual-parameter statement) environment return break continue throw) environment)))
      (call/cc
       (lambda (return)
         (interpret-statement-list (function-body closure) new-environment return default-break default-continue throw)))) ))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
 (lambda (expr environment)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment)))))


;-----------------
; HELPER FUNCTIONS
;-----------------

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
  (lambda (stmt)
    (list? (car stmt)) ))

; These helper functions define the parts of the various statement types
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

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

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


;------------------------
; Environment/State Functions
;------------------------

; Remove return in a code when return is not needed.
(define removereturn
  (lambda (statement)
    (cond
      ((null? statement) '())
      ((eq? (caar statement) 'return) (cdr statement))
      (else (cons (car statement) (removereturn (cdr statement)))))))

; Create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; Create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; Add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; Remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; Some abstractions which represent the top and rest frame of environment
(define topframe car)
(define remainingframes cdr)

; Does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; Does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

(define lookup-val
  (lambda (var environment)
    (unbox (lookup-in-env var environment)) ))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var (box val) (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (begin (set-box! (lookup-in-env var environment) val) environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Does the current frame contain input variable?
(define frame-contains?
  (lambda (variable frame)
    (if (null? frame)
        #f
        (exists-in-list? variable (variables frame)))))

; Return the frame of the input environment which contains the name of function(fname) 
(define get-function-frame
  (lambda (fname environment)
    (cond
      ((null? environment) (myerror "error: undefined function:" fname))
      ((frame-contains? fname (topframe environment)) environment)
      (else (get-function-frame fname (remainingframes environment))))))

; Create a newe frame with parameters
(define new-frame-parameter
  (lambda (formal actual environment return break continue throw)
    (cond
      ((and (null? formal) (null? actual)) (newframe))
      ((or (null? formal) (null? actual)) (myerror: "error: mismatch"))
      (else (add-to-frame (first-para formal) (box (interpret-value (first-para actual) environment return break continue throw)) (new-frame-parameter (rest-para formal) (rest-para actual) environment return break continue throw)))
      )))


; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.
(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

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
