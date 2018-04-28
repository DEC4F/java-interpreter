;base layer
(define base
  (lambda ()
    '(((true false) (#t #f)))))

;add another layer
(define push_layer
  (lambda (s)
    (cons '(() ()) s)))

;remove upmost layer 
(define pop_layer
  (lambda (s)
    (rest s)))

; Update the current state by adding a variable and value to the layer. 
(define add_to_state
  (lambda (var value s)
    (if (eq? 'undeclared value)
        (error "Error Code: USING_BEFORE_DECLARING" var)
        (call/cc
         (lambda (break)
           (add_to_state_help var value s (lambda () (break (cons (add_to_layer var value (first s)) (rest s))))))))))

; Method used to check whether a variable is defined before in the state
(define lookup_list
  (lambda (var s)
    (cond
      ((null? s) 'undeclared)
      ((null? (first s)) (lookup_list var (rest s)))
      ((not (eq? 'undeclared (lookup var (first s)))) (lookup var (first s)))
      (else (lookup_list var (rest s))) )))

;--------------------------------------------The followings are helper method------------------------------------

; Helper method used by addtos. It helps modify the state by adding a variable name and value to each layer.
(define add_to_layer
  (lambda (var value s)
    (cond
      ((null? (first s))
       (cons (append (first s) (cons var '())) (cons (append (second s) (cons value '())) '())))
      ((eq? (car (first s)) var)
       (cons (first s)  (cons (cons value (cdr (second s))) '())) )
      (else (cons (cons (car (first s)) (car (add_to_layer var value (cons (cdr (first s)) (cons (cdr (second s)) '()))) )) (cons (cons (car (second s)) (car (cdr (add_to_layer var value (cons (cdr (first s)) (cons (cdr (second s)) '()))) ))) '()) )))))
; Helper method used by addtos
(define add_to_state_help
  (lambda (var value s break)
    (cond
      ((null? s) (break))
      ((not (eq? 'undeclared (lookup var (first s)))) (cons (add_to_layer var value (first s)) (rest s)))
      (else (cons (first s) (add_to_state_help var value (rest s) break))))))


; Helper method to lookup the variable in the current state 
(define lookup
  (lambda (var s)
    (cond
      ((or (null? s) (null? (first s))) 'undeclared)
      ((eq? var (first (first s))) (first (second s)))
      (else (lookup var (cons (rest (first s)) (cons (rest (second s)) '())))) )))