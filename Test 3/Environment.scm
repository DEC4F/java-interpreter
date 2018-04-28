;------------------------
; Frame Operation Function
;------------------------

; Create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (append (store frame) (list val)))))

; Add an environment
(define add-env
  (lambda (var val env)
    (add-to-frame var (box val) env)))


; Get the value in frame
(define lookup-in-frame
  (lambda (var frame)
    (let ((index (indexof var (variables frame))))
          (cond
            ((= index -1) 'no_value)
            (else (list-ref (store frame) (- (length (variables frame)) index 1)))) )))

; Get the value in environment and unbox it
(define lookup-in-env
  (lambda (var env)
    (let ((value (lookup-in-frame var env)))
      (cond
        ((eq? value 'no_value) 'no_value)
        (else (unbox value)) ))))

; Check whether the frame contain variable
(define check-frame
  (lambda (var frame)
    (not (= -1 (indexof var (car frame))))))

;------------------------
; Environment/State Functions
;------------------------
(define removeframe cdr)
(define firstframe car)
(define restframe cdr)

; a new empty environment
(define newenv
  (lambda ()
    '((() ()))))

; Add an empty frame into environment
(define addframe
  (lambda (env)
    (cons (newframe) env)))

; Add variable and value into environment
(define add-to-env
  (lambda (var value env)
    (cond
      ((check-frame var (car env)) (myerror "error: variable is being re-declared:" var))
      (else (cons (add-to-frame var (box value) (firstframe env)) (restframe env))) )))

; Get the environment for the function 
(define get-env
  (lambda (f env)
    (cond
      ((null? env) (myerror "error: can not find function:" var))
      (else (let ((value (lookup-in-frame f (firstframe env))))
              (cond
                ((eq? value 'no_value) (get-env f (restframe env)))
                (else env)))))))

; Check and get value in environment
(define check-binding
  (lambda (var env)
    (unbox (check-box var env))))
(define check-box
  (lambda (var env)
    (let ((value (get-binding var env)))
      (cond
        ((eq? value 'no_value) (myerror "error: binding is not found for variable:" var))
        (else value)))))
(define get-binding
  (lambda (var env)
    (cond
      ((null? env) 'no_value)
      (else (let ((value (lookup-in-frame var (firstframe env))))
              (cond
                ((eq? value 'no_value) (get-binding var (restframe env)))
                (else value)))))))

; Check whether the environment contains the variable
(define contains?
  (lambda (var env)
    (not (eq? (get-binding var env) 'no_value))))
