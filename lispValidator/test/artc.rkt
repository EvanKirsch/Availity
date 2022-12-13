#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts a scheme-expression into a string
;; INPUT: a scheme-expression EXP
;; OUTPUT: a SCHEME String corresponding to EXP
(define (exp->string exp)
  (cond ((number? exp) (number->string exp))
        ((symbol? exp) (symbol->string exp))
        ((list? exp) (exp->string (car exp)))))

;; INPUT: a list of lists
;; OUTPUT: a list containing all elements of the first-level lists
(define (flatten list-of-lists)
  (cond ((null? list-of-lists) '())
        (else (append (car list-of-lists) (flatten (cdr list-of-lists))))))

;; this is for all error handling.
;; programmers don't use this function but
;; the interpreter calls this function to
;; signal some type of programmer error
(define (error msg)
  (display "ERROR: ")
  (display msg)
  (newline))

;; THERE ARE TWO SUPPORTED TYPES: 'int and 'boolean
;; INPUT: an element of the ART-C language
;; OUTPUT: the type of that element
(define (type-of val)
  (cond ((number? val) 'int)
        ((boolean? val) 'boolean)))

;; A MAP is a list of key-value pairs
;; INPUT: a MAP and a KEY
;; OUTPUT: The value associated with the key or 'error
(define (map-get map x)
  (cond ((null? map) 'error)
        ((equal? (car (car map)) x) (cadr (car map)))
        (else (map-get (cdr map) x))))

;; INPUT : A MAP AND KEY
;; OUTPUT : true if the key is in the map and false otherwise
(define (map-contains map x)
  (cond ((null? map) #f)
        ((equal? (car (car map)) x) #t)
        (else (map-contains (cdr map) x))))

;; INPUT : A MAP, KEY and VALUE
;; OUTPUT: The map that results from replacing the key with the new value.  If
;; the map doesn't contain KEY, then 'error is returned
(define (map-replace map key val)
  (cond ((null? map) 'error)
        ((equal? (car (car map)) key)
         (cons (list key val) (cdr map)))
        (else
         (cons (car map) (map-replace (cdr map) key val)))))

;; INPUT : A MAP, Key and Value
;; OUTPUT : The map that results from adding a key-value pair.  This
;; allows for duplicate keys (the most-recently added is nearer the front of the list
(define (map-add map key val)
  (cons (list key val) map))

;; INPUT: A MAP and KEY
;; OUTPUT: The map that results from deleting the key.  No errors occur if the map
;; doesn't contain the key
(define (map-delete map key)
  (cond ((null? map) map)
        ((equal? (car (car map)) key) (cdr map))
        (else (cons (car map)
                    (map-delete (cdr map) key)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPEMAP : A SEMANTIC DOMAIN DATA TYPE
;; A typemap is a list of block-level declarations.
;; FORM: ((var1 type1) (var2 type2) (var3 type3) ... )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT: NONE
;; OUTPUT: AN empty typemap
(define (typemap-create-empty) '())

;; INPUT: A TYPEMAP
;; OUTPUT: The type of variable x
(define (typemap-type-of tm x)
  (map-get tm x))

;; INPUT: A TYPEMAP
;; OUTPUT: THE TYPEMAP THAT RESULTS FROM INSERTING A DECLARATIONS
(define (typemap-add tm decl)
  (map-add tm (caddr decl) (cadr decl)))      

(define (typemap-delete tm key)
  (map-delete tm key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE : A SEMANTIC DOMAIN DATA TYPE
;; A LIST OF (VAR, VALUE) pairs
;; FORM :  ( (var1 val1) (var2 val2) ... )
;; NOTE: A map can contain duplicate keys but innermost KEYS occur
;;       before outermost KEYS and hide them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INPUT : NONE
;; OUTPUT: AN EMPTY STATE
(define (state-create-empty) '())
  
;; INPUT: STATE and ID
;; OUTPUT: a new state such that the innermost scope now contains a
;;         new binding for the specified ID.  The bindings value is 'undefined.
(define (state-add state id)
  (map-add state id 'undefined))

;; INPUT : STATE and ID
;; OUTPUT: A new state such that the innermost id is removed
(define (state-delete state id)
  (map-delete state id))

;; INPUT: STATE and ID
;; OUTPUT: The value associated with the specified ID in the given state
(define (state-get-value state id)
  (map-get state id))

;; INPUT: STATE and ID
;; OUTPUT: A new state that results from changing the mapping from id->value in
;;         the specified state
(define (state-update state id value)
  (map-replace state id value))

;; INPUT: STATE and LIST-OF-IDS (VARIABLES)
;; OUTPUT: A new state that results from deleting all ids (the variables) from
;;         the specified state
(define (state-delete-all state variables)
  (cond ((null? variables) state)
        (else (state-delete-all (state-delete state (car variables)) (cdr variables)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THESE CLASSES CORRESPOND TO THE ABSTRACT SYNTAX SUCH THAT A "PROGRAM"
;; REPRESENTS A PARSE-TREE.  THESE FUNCTIONS OPERATE AT THE 'SYNTACTIC' LEVEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (PROGRAM BODY)
(define (program-get-body stmt)
  (cadr stmt))

;; (BLOCK S1...SN)
(define (block-get-body stmt)
  (filter (lambda (x) (not (is-declaration? x))) (cdr stmt)))
  
(define (block-get-declarations stmt)
  (filter (lambda (x) (is-declaration? x)) (cdr stmt)))

;; (DECLARE TYPE VAR)
(define (declaration-get-type stmt)
  (cadr stmt))

(define (declaration-get-var stmt)
  (caddr stmt))

(define (is-declaration? stmt)
  (and (list? stmt) 
       (equal? (car stmt) 'declare)))

;; (:= VAR EXP)
(define (assignment-get-var stmt)
  (cadr stmt))

(define (assignment-get-exp stmt)
  (caddr stmt))

;; (IF TEST THEN [ELSE])
(define (if-get-test stmt)
  (cadr stmt))

(define (if-get-then stmt)
  (caddr stmt))

(define (if-has-else? stmt)
  (= (length stmt) 4))

(define (if-get-else stmt)
  (cadddr stmt))

;; (WHILE TEST BODY)
(define (while-get-test stmt)
  (cadr stmt))

(define (while-get-body stmt)
  (caddr stmt))

;; (SPRINT LABEL EXP)
(define (sprint-has-exp? stmt)
  (and (list? stmt)
       (= (length stmt) 3)))

(define (sprint-get-label? stmt)
  (cadr stmt))

(define (sprint-get-exp stmt)
  (caddr stmt))

;; INPUT: an expression EXP
;; OUTPUT: the operator of EXP (an element of ART-C)
(define (exp-get-operator exp)
  (car exp))

;; INPUT: an expression EXP
;; OUTPUT: the left-operand (an expression) of EXP
(define (exp-get-left-operand exp)
  (car (cdr exp)))

;; INPUT: an expression EXP
;; OUTPUT: the exp-get-right-operand (an expression) of EXP
(define (exp-get-right-operand exp)
  (car (cdr (cdr exp))))

;; INPUT: an expression EXP
;; OUTPUT: #t if the expression is a boolean literal and #f otherwise
(define (bool? exp)
  (or (equal? exp 'true)
      (equal? exp 'false)))

;; INPUT: a symbol
;; OUTPUT: #t if the symbol is 'true and #f if it is 'false and 'void' if neither
(define (symbol->bool sym)
  (cond ((equal? sym 'true) #t)
        ((equal? sym 'false) #f)))


;; INPUT: A PROGRAM
;; A PROGRAM has syntactic structure (program stmt)
;; OUTPUT: THE STATE that results from executing the program
;;         in an empty state.
(define (interpret-program pgm)
  (interpret (program-get-body pgm) (state-create-empty) (typemap-create-empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the INTERPRETER class
;; An INTERPRETER is simply a collection of functions that
;; operates on TYPES, STATES, BINDING, SCOPES and PROGRAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT: STATEMENT and STATE
;; OUTPUT: The state that results from executing STATEMENT in STATE
(define (interpret stmt state tm)
  (display stmt) (newline) (display state) (newline)
  (let ((kind (car stmt)))
    (cond ((or (equal? state 'error) (equal? tm 'error)) 'error)
          ((equal? kind 'block) (interpret-block stmt state tm))
          ((equal? kind 'declare) (interpret-declaration stmt state tm))
          ((equal? kind ':=) (interpret-assignment stmt state tm))
          ((equal? kind 'if) (interpret-if stmt state tm))
          ((equal? kind 'sprint) (interpret-sprint stmt state tm))
          ((equal? kind 'while) (interpret-while stmt state tm))       
          (else (error (string-append "statement expected but saw (" (exp->string stmt) "...) instead."))))))


(define not-valid-pgm '(program 
              (block
               (declare int n)
               (declare boolean error)
               (declare int result)
               (:= error false)
               (:= result 1)
               (block 
                (declare int local)
                (:= n 2)
                (:= local n)
                (while (> local 0)
                       (block
                        (:= result (* result local))
                        (:= local (- local 1))
                        (:= error (! error error)))))
              (sprint "result: " result)
              (if (~ error) (sprint "a") (sprint "b")))))

(define valid-pgm '(program 
              (block
               (declare int n)
               (declare boolean error)
               (declare int result)   
               (:= error false)
               (:= result 1)
               (block 
                (declare int local)
                (:= n 5)
                (:= local n)
                (while (> local 0)
                       (block
                        (:= result (* result local))
                        (:= local (- local 1)))))
              (sprint "result: " result)
              (if (~ error) (sprint "a") (sprint "b")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     Start of hw3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-program-valid pgm)
        (is-block-valid (program-get-body pgm) (typemap-create-empty)))

(define (is-block-valid block tm)
        (and (equal? (car block) 'block )
             (all-declarations-valid (block-get-declarations block))
             (let ([tmap (map-declarations (block-get-declarations block)tm)])
               (cond ((not tmap) #f)
                     (else (is-body-valid (block-get-body block) tmap))))))                                     

(define (all-declarations-valid decls)
        (cond ((null? decls) #t)
              ((is-declaration? (car decls)) (and (is-declaration-valid (car decls)) (all-declarations-valid (cdr decls))))
              (else #f )))
 
(define (is-declaration-valid decl)
        (and (is-declaration? decl)
             (is-type-valid (declaration-get-type decl))
             (is-variable-valid (declaration-get-var decl))))

(define (map-declarations decls tm)
        (cond ((null? decls) tm )
              ((map-contains tm (caddr (car decls))) #f)
              (else (map-declarations (cdr decls) (typemap-add tm (car decls))))))

(define (is-type-valid type)
        (or (equal? type 'boolean) (equal? type 'int)))

(define (is-variable-valid variable)
        (cond ((equal? variable 'program) #f)
              ((equal? variable 'block) #f)
              ((equal? variable 'declare) #f)
              ((equal? variable 'if) #f)
              ((equal? variable 'while) #f)
              ((equal? variable 'sprint) #f)
              ((equal? variable 'boolean) #f)
              ((equal? variable 'int) #f)
              ((equal? variable 'true) #f)
              ((equal? variable 'false) #f)
              (else (let ([str (string->list (exp->string variable))])
                    (and (char-alphabetic? (car str)) (valid-var? (cdr str)))))))

(define (valid-var? str)
        (cond ((null? str) #t)
              (else (and (not (char-whitespace? (car str))) (valid-var? (cdr str))))))
         
(define (is-body-valid statements tm)
        (cond ((null? statements) #t)
              (else (and (is-statement-valid (car statements) tm) (is-body-valid (cdr statements) tm)))))

(define (is-statement-valid statement tm)
        (cond ((null? (car statement)) #t)
              ((equal? (car statement) 'block) (is-block-valid statement tm))
              ((equal? (car statement) ':=) (is-assign-valid statement tm))
              ((equal? (car statement) 'if) (is-if-valid statement tm))
              ((equal? (car statement) 'while) (is-while-valid statement tm)) 
              ((equal? (car statement) 'sprint) (is-sprint-valid statement)) 
              (else #f)))

(define (is-assign-valid assign tm)
        (and (equal? (car assign) ':=)
             (is-variable-valid (assignment-get-var assign))
             (is-expression-valid (assignment-get-exp assign))
             (equal? (typemap-type-of tm (assignment-get-var assign)) (expression-type tm (assignment-get-exp assign)))))

(define (is-if-valid stmt tm)
        (cond ((equal? (length stmt) 4)
                       (and (equal? (car stmt) 'if)
                            (and (is-expression-valid (cadr stmt)) (equal? 'boolean (expression-type tm (cadr stmt))))
                            (is-statement-valid (caddr stmt) tm)
                            (is-statement-valid (cadddr stmt) tm)))
              (else (and (equal? (car stmt) 'if)
                         (is-expression-valid (cadr stmt))
                         (is-statement-valid (caddr stmt) tm)))))

(define (is-while-valid stmt tm)
        (and (equal? (car stmt) 'while)
             (and (is-expression-valid (cadr stmt)) (equal? 'boolean (expression-type tm (cadr stmt))))
             (is-statement-valid (caddr stmt) tm)))

(define (is-sprint-valid stmt)
        (cond ((equal? (length stmt) 3)
                       (and (equal? (car stmt) 'sprint)
                            (string? (cadr stmt))
                            (is-expression-valid (caddr stmt))))
              (else (and (equal? (car stmt) 'sprint) (string? (cadr stmt))))))

(define (expression-type tm expression)
             (cond ((list? expression) (expression-type-helper tm expression))
                   ((integer? expression) 'int)
                   ((bool? expression) 'boolean)
                   (else (typemap-type-of tm expression))))

(define (expression-type-helper tm expression)
        (cond ((equal? (op-arity (car expression)) 1) (expression-type tm (cadr expression)))
              (else (cond ((and (equal? (expression-type tm (cadr expression))
                                        (expression-type tm (caddr expression)))
                                (equal? (expression-type tm (cadr expression))
                                        (op-operand-type (car expression))))
                                (op-return-type (car expression)))
                          (else #f)))))

(define (is-expression-valid expression)
        (cond ((not (list? expression)) (or (integer? expression) (bool? expression) (is-variable-valid expression)))
              ((equal? (length expression) '1) (or (integer? (car expression)) (bool? (car expression)) (is-variable-valid (car expression))))
              ((equal? (car expression) '~) (is-expression-valid (cdr expression)))
              ((and (equal? (car expression) '-) (equal? (length expression) 2)) (is-expression-valid (cdr expression)))
              (else (is-binary-valid expression))))

(define (is-binary-valid exp)
        (and (is-op-valid (car exp))
             (is-expression-valid (cadr exp))
             (is-expression-valid (caddr exp))))

(define (is-op-valid op)
        (or (equal? op '+)
            (equal? op '-)
            (equal? op '*)
            (equal? op '/)
            (equal? op '@)
            (equal? op '?)
            (equal? op '<)
            (equal? op '>)
            (equal? op '=)
            (equal? op '<=)
            (equal? op '>=)
            (equal? op '&)
            (equal? op '%)))

(define (op-return-type op)
        (cond ((or (equal? op '+)
                   (equal? op '-)
                   (equal? op '*)
                   (equal? op '/)
                   (equal? op '@)
                   (equal? op '?)
                   (equal? op '~)) 'int)
              ((or (equal? op '<)
                   (equal? op '>)
                   (equal? op '=)
                   (equal? op '<=)
                   (equal? op '>=)
                   (equal? op '&)
                   (equal? op '%)) 'boolean)
              (else #f)))

(define (op-operand-type op)
        (cond ((or (equal? op '+)
                   (equal? op '-)
                   (equal? op '*)
                   (equal? op '/)
                   (equal? op '@)
                   (equal? op '?)
                   (equal? op '~)
                   (equal? op '<)
                   (equal? op '>)
                   (equal? op '=)
                   (equal? op '<=)
                   (equal? op '>=)) 'int)
              ((or (equal? op '&)
                   (equal? op '%)) 'boolean)
              (else #f)))

(define (op-arity op)
        (cond ((equal? op '~) 1)
              (else 2)))

(define (integer? int)
        (cond ((not (number? int)) #f)
              (else (let ([num (string->list (number->string int))])
                         (and (or (char-numeric? (car num)) (equal? '- (car num))) (integer?-helper (cdr num)))))))

(define (integer?-helper int)
        (cond ((null? int) #t)
              (else (and (char-numeric? (car int)) (integer?-helper (cdr int))))))
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                       Interperter
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                         
(define (interpret-block stmt state tm)
        (cond ((or (equal? state 'error) (equal? tm 'error)) 'error)
              (else (interpret-body (block-get-body stmt) (read-declarations-state (block-get-declarations stmt) state) (read-declarations-tm (block-get-declarations stmt) tm)))))

(define (interpret-body stmt state tm)
        (cond ((null? stmt) state)
              ((or (equal? state 'error) (equal? tm 'error)) 'error)
              (else (interpret-body (cdr stmt) (interpret (car stmt) state tm) tm))))
        
(define (read-declarations-tm decls tm)
        (cond ((null? decls) tm)
              ((equal? tm 'error) 'error)
              (else (read-declarations-tm (cdr decls) (typemap-declaration (car decls) tm)))))

(define (typemap-declaration decl tm)
        (cond ((not (is-declaration-valid decl)) (error "Invalid Declaration: Unable to add to Type-Map") 'error)
              ((map-contains tm (declaration-get-var decl)) (error "Invalid Declaration: Variable Already in Type Map") 'error)
              (else (typemap-add tm decl))))                              

(define (read-declarations-state decls state)
        (cond ((null? decls) state)
              ((equal? state 'error) 'error)
              (else (read-declarations-state (cdr decls) (interpret-declaration (car decls) state)))))

(define (interpret-declaration stmt state)
        (cond ((not (is-declaration-valid stmt)) (error "Invalid Declaration: Unable to add to State") 'error)
              ((map-contains state (declaration-get-var stmt)) (error "Invalid Declaration: Variable Already in State") 'error)
              (else (state-add state (declaration-get-var stmt)))))

(define (interpret-assignment stmt state tm)
        (cond ((null? stmt) state)
              ((not (is-expression-valid (assignment-get-exp stmt))) (error "Invalid Assignment: Expression Invalid") 'error)
              ((not (map-contains state (assignment-get-var stmt))) (error "Invalid Assignment: Variable Not in State") 'error)
              ((not (is-assign-valid stmt tm)) (error "Invalid Assignment: Type Miss-Match") 'error)
              (else (state-update state (assignment-get-var stmt) (evaluate-expression state (assignment-get-exp stmt))))))

(define (interpret-if stmt state tm)
        (cond ((not (is-if-valid stmt tm)) (error "Invalid If Statement") 'error)
              ((evaluate-expression state (if-get-test stmt)) (interpret (if-get-then stmt) state tm))
              ((if-has-else? stmt) (interpret (if-get-else stmt) state tm))
              (else state)))
              
(define (evaluate-expression state expr)
        (cond ((not (is-expression-valid expr)) (error "Invalid Expression: Could Not Evaluate Expression") 'error)
              ((and (not (list? expr)) (map-contains state expr)) (state-get-value state expr))
              ((not (list? expr)) expr)
              ((equal? (op-arity (car expr)) 1) (not (evaluate-expression state (cadr expr))))
              (else (evaluate-op state (car expr) (cadr expr) (caddr expr)))))

(define (evaluate-op state op e1 e2)
          (cond ((equal? op '+) (+ (evaluate-expression state e1) (evaluate-expression state e2)))
                ((equal? op '-) (- (evaluate-expression state e1) (evaluate-expression state e2)))
                ((equal? op '*) (* (evaluate-expression state e1) (evaluate-expression state e2)))
                ((equal? op '/) (/ (evaluate-expression state e1) (evaluate-expression state e2)))
                ((equal? op '@) (expt (evaluate-expression state e1) (evaluate-expression state e2)))
                ((equal? op '?) (remainder (evaluate-expression state e1) (evaluate-expression state e2)))
                ((equal? op '<) (< (evaluate-expression state e1) (evaluate-expression state e2)))
                ((equal? op '>) (> (evaluate-expression state e1) (evaluate-expression state e2)))
                ((equal? op '=) (equal? (evaluate-expression state e1) (evaluate-expression state e2)))
                ((equal? op '<=) (<= (evaluate-expression state e1) (evaluate-expression state e2)))
                ((equal? op '>=) (>= (evaluate-expression state e1) (evaluate-expression state e2)))
                ((equal? op '&) (and (evaluate-expression state e1) (evaluate-expression state e2)))
                ((equal? op '%) (or (evaluate-expression state e1) (evaluate-expression state e2)))
                (else (error "OP Not Found: Unable to Evaluate") 'error)))
  
(define (interpret-sprint stmt state tm)
        (cond ((not (is-sprint-valid stmt)) (error "Invalid Sprint") 'error) 
              ((not (sprint-has-exp? stmt)) (display state) (newline) state)
              (else (display (sprint-get-label? stmt)) (display (evaluate-expression state (sprint-get-exp stmt))) (newline) state)))
               
(define (interpret-while stmt state tm)
        (cond ((not (is-while-valid stmt tm)) (error "Invalid While Loop") 'error)
              ((evaluate-expression state (while-get-test stmt)) (interpret-while stmt (interpret (while-get-body stmt) state tm) tm))
              (else state)))
