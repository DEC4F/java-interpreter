;------------------------
; Interpret class part
;------------------------

(define param caddr)
(define bod cadddr)
  
(define interpret-class
  (lambda (statement env collection)
    (let* ((name (get-declare-var statement))
           (extends (get-declare-value statement))
           (parent (if (null? extends) 'null (check-binding (binding extends) env)))
           (body (get-funcbody statement))
           (init (newclass parent name))
           (class (M-class-statement body env (set-current init (set-class init collection) ))))
      (add-to-env name class env)) ))

(define M-class-static-declare
  (lambda (statement env collection)
    (let* ((class (class-in-c collection)))
      (set-class-field (add-env (cadr statement)
                               (if (= 3 (length statement)) (interpret-value (param statement) env collection) 'undefined) (class-field class)) class) )))

; Returns the binding after class declaration
(define M-class-declare
  (lambda (statement env collection)
    (let* ((class (class-in-c collection)))
      (set-class-instance (add-to-frame (cadr statement)
                     (if (= 3 (length statement)) (interpret-value (param statement) env collection) 'undefined) (instance-of-class class)) class) )))


; Adds the function to the class and returns it
(define M-class-func-declare
  (lambda (f env static collection)
    (let* ((fname (cadr f))
           (class (class-in-c collection))
           (cname (class-name class)))
      (set-class-method
       (add-env fname
                (list (param f) (bod f)
                      (lambda (env) 
                        (let ((class (check-binding cname env)))
                          (get-env cname env)))
                      (lambda (env)   
                        (check-binding cname env))
                      (if static (lambda (v) 'null) (lambda (v) v))) (class-method class)) class) )))

; Executes each statement and returns the class
(define M-class
  (lambda (statement env collection)
    (cond
     ((null? statement) (class-in-c collection))
     ((eq? 'static-function (statement-type statement)) (M-class-func-declare statement env #t collection))
     ((eq? 'function (statement-type statement)) (M-class-func-declare statement env #f collection))
     ((eq? 'static-var (statement-type statement)) (M-class-static-declare statement env collection))
     ((eq? 'var (statement-type statement)) (M-class-declare statement env collection))
     ((list? statement) (myerror "error: invalid statement: " statement))
     (else (class-in-c collection)) )))

; interprets each statement in class declaration
(define M-class-statement
  (lambda (block env collection)
    (cond
      ((null? block) (class-in-c collection))
      (else (M-class-statement (cdr block) env
                               (let ((newclass (M-class (car block) env collection)))
                                 (set-current newclass (set-class newclass collection))) )) )))


 
;------------------------
; Class related abstraction and helper method
;------------------------
(define object car)
(define class-parent cadr)
(define class-name caddr)
(define class-field cadddr)
(define class-instance cadr)
(define instance-value caddr)

(define newclass
  (lambda (parent name)
    (list 'class parent name
          (if (eq? parent 'null) (newframe) (class-field parent))
          (if (eq? parent 'null) (newframe) (class-method parent))
          (if (eq? parent 'null) (newframe) (instance-of-class parent)) )))

; Creates a new instance of the class. 
(define newinstance
  (lambda (class)
    (list 'inst class '())))

(define class-method
 (lambda (l)
  (list-ref l 4)))

(define instance-of-class 
	(lambda (l) 
		(list-ref l 5)))

(define set-class-field
  (lambda (fields class)
    (replace 3 fields class)))

(define set-class-method
  (lambda (methods class)
    (replace 4 methods class)))

(define set-class-instance
  (lambda (inst class)
    (replace 5 inst class)))

; Modify an instance.
(define set-instant-value
  (lambda (inst values)
    (list 'inst (class-instance inst) values)))


(define find-var-box
  (lambda (var env class instance)
    (cond
     ((eqv? var 'this) (box instance))
     ((contains? var env) (check-box var env))
     ((and (not (eq? 'null class)) (check-frame var (class-field class))) (lookup-in-frame var (class-field class)))
     ((and (not (eq? 'null instance)) (check-frame var (list (car (instance-of-class class)) (instance-value instance))))
      (lookup-in-frame var (list (car (instance-of-class class)) (instance-value instance))))
     (else 'no_value))))

; Function to look up the value using the envionment 
(define find-func-env
  (lambda (var env class instance)
    (cond
     ((contains? var env) (check-binding var env))
     ((check-frame var (class-method class)) (lookup-in-env var (class-method class)))
     (else (myerror "error: can not find function: " var)))))

; Return the class contained by the variable 
(define find-var-class
  (lambda (var env collection)
    (cond
     ((not (list? var)) (myerror "error: wrong format for dot: " var))
     ((eq? (object var) 'class) (list 'null var))
     ((eq? (object var) 'inst) (list var (class-instance var)))
     (else (myerror "error: wrong use of dot operator!")))))

; Converts the symbols to what they acctually are and adds to a pair 
(define convert
  (lambda (statement env class collection)
    (if (list? statement)
        (cond
         ((eq? (car statement) 'funcall) (find-var-class (M-value-funcall statement env collection) env collection))
         ((eq? (car statement) 'dot) (find-var-class (unbox (searchDotVar statement env collection)) env collection))
         ((eq? (car statement) 'new) (find-var-class (M-value-new statement env collection)  env collection)))
        (let ((lookup (find-var-box statement env class (instance-in-c collection))))
          (cond
           ((eq? statement 'this) (list (instance-in-c collection) (class-instance (instance-in-c collection))))
           ((eq? statement 'super) (list (instance-in-c collection) (class-parent class)))
           ((eq? 'no_value lookup) (myerror "error: can not found variable"))
           (else (find-var-class (unbox lookup) env collection)))) )))

; Return the dot function within the statement 
(define find-dot-func
  (lambda (statement env collection)
    (let ((class-instance (convert (function-name statement) env (class-in-c collection) collection)))
      (cons (find-func-env (function-env statement) (newenv) (function-name class-instance) (object class-instance)) class-instance) )))

; Looks for the dot function given the statement 
(define find-func
  (lambda (statement env collection)
    (cond
      ((list? statement) (find-dot-func statement env collection))
      (else (list (find-func-env statement env (class-in-c collection) (instance-in-c collection)) (instance-in-c collection) (class-in-c collection))) )))

; Looks up the dot variable associated with the dot func 
(define find-dot-var
  (lambda (statement env collection)
    (let ((instance (convert (class-instance statement) env (current-in-c collection) collection)))
      (find-var-box (function-env statement) (newenv) (class-parent instance) (car instance)) )))

; Looks up the variable associated with the function 
(define find-var
  (lambda (statement env collection)
    (cond
      ((list? statement) (find-dot-var statement env collection))
      (else (find-var-box statement env (current-in-c collection) (instance-in-c collection))) )))
