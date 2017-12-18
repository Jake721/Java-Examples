; Jacob Ruth ( ruth.105 )
; Project 4
; Due: 11/30/17

; myinterpreter is called with a list of programs.
; for each program it finds it calls eval-prog
; eval-prog strips off the 'prog' keyword and evaluates the first expression
; eval-expr is where most of the logic happens. This function checks to see
; if the expression is a integer? if so simply return the value
; if the expression is a symbol? call our eval-binding function
; for myadd, myneg, myignore, mylet call its respective function
 
; each 'my' function is very similar with exception of mylet. They each call
; their respective built in functions from scheme

; mylet stores the value of the first expression and the id within a list
; Ex: '(x 10) then stores that within the list of bindings

; eval-bindings searches through our bindings looking for a matching id
; then returns the value associated with it


; The main method
(define (myinterpreter lst)
  (cons (cond ((not (null? (car lst)))  (eval-prog (car lst)) ))
        (cond ((not (null? (cdr lst)))  (myinterpreter (cdr lst)) )
          ((null? (cdr lst)) '())
        )
  )
)

; Strips off the prog keyword and calls eval-expr
(define (eval-prog l)
    (cond ((null? l) "Empty program")
      ((equal? (car l) 'prog) (eval-expr (cdr l) '() ))     
      (not (equal? (car l) 'prog) "Expected prog")
    )
)

; Detects what type of expression the current keyword is and calls its respective function
(define (eval-expr expr bind)
    (cond ((null? expr) "Empty expr")
      ((integer? (car expr)) (car expr))
      
      ((symbol? (car expr)) (eval-bindings (car expr) 
                                           bind))
                                         
      ((equal? (car (car expr)) 'myignore) 0)
                                                  
      ((equal? (car (car expr)) 'myadd) (eval-myadd (list(car(cdr(car expr)))) 
                                                    (list(car(cdr(cdr(car expr)))))
                                                    bind))
                                                  
      ((equal? (car (car expr)) 'mymul) (eval-mymul (list(car(cdr(car expr)))) 
                                                    (list(car(cdr(cdr(car expr)))))
                                                    bind))
                                                  
      ((equal? (car (car expr)) 'myneg) (eval-myneg (car expr)) 
                                                    bind)
                                                  
      ((equal? (car (car expr)) 'mylet) (eval-mylet (list(car(cdr(car expr)))) 
                                                    (list(car(cdr(cdr(car expr))))) 
                                                    (list(car(cdr(cdr(cdr(car expr))))))
                                                    bind))
    )
)

; Finds and returns the value of a given identifier
(define (eval-bindings id bind)
  (cond ((null? bind) 0)
    ((equal? id (car(car bind))) (cdr(car bind)) )
    ((not (null? (cdr bind))) (eval-bindings id bind))
  )
)

; Adds the two incoming expressions
(define (eval-myadd expr1 expr2 bind)
  (+ (eval-expr expr1 bind) (eval-expr expr2 bind))
)

; Multiplies the two incoming expressions
(define (eval-mymul expr1 expr2 bind)
  (* (eval-expr expr1 bind) (eval-expr expr2 bind))
)

; Negates the incoming expression
(define (eval-myneg expr bind)
  (* -1 (eval-expr (cdr expr) bind))
)

; Adds a pair to the bindings list
(define (eval-mylet id expr1 expr2 bind)
  (let ((temp (eval-expr expr1 bind)))
       (eval-expr expr2 (cons (cons (car id) temp) bind)))
)
