#lang racket
(require racket/trace)
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

;; SCANNER
(define input (string->list (file->string "input01.txt")))

(define (scanner i)
  (cond
    [(empty? i) (error "Error: ran out of file before seeing an EOF marker ($$).")]
    [(and (equal? (first i) #\$) (equal? (second i) #\$) '('eof))]
    ; checks to see if file input matches "read" then 
    [(and (equal? (first i) #\r) (equal? (second i) #\e) (equal? (third i) #\a) (equal? (fourth i) #\d)) (cons 'Read (scanner (rest (rest (rest (rest i))))))]
    [(and (equal? (first i) #\w) (equal? (second i) #\r) (equal? (third i) #\i) (equal? (fourth i) #\t) (equal? (fifth i) #\e)) (cons 'Write (scanner (rest (rest (rest (rest (rest i)))))))]
    [(char-numeric? (first i)) (cons 'Num (scanner (rest i)))]
    [(char-alphabetic? (first i)) (cons 'ID (scanner (rest i)))]
    [(or (equal? (first i) #\space) (equal? (first i) #\return) (equal? (first i) #\newline)) (scanner (rest i))]
    [(and (equal? (first i) #\:) (equal? (second i) #\=)) (cons 'Eq (scanner (rest (rest i))))]
    [(equal? (first i) #\=) (cons 'Eq (scanner (rest (rest i))))]
    [(equal? (first i) #\() (cons 'Lparen (scanner (rest i)))]
    [(equal? (first i) #\)) (cons 'Rparen (scanner (rest i)))]
    [(equal? (first i) #\+) (cons 'Plus (scanner (rest i)))]
    [(equal? (first i) #\*) (cons 'Multiply (scanner (rest i)))]
    [(equal? (first i) #\-) (cons 'Minus (scanner (rest i)))]
    [(equal? (first i) #\/) (cons 'Divide (scanner (rest i)))]

    
    [else (error (format "Unrecognized token: ~a" (first i)))]
    )
  
  )

(define (match type i)
  (if
   (equal? type (first i))
   (rest i)
   (error "Syntax error")))
  
(define (program i)
  (case (first i)
  [(ID Read Write eof) (match 'eof (stmt_list i))]
  [else (error "Syntax error")]))

(define (stmt_list i )
  (case (first i)
  [(ID Read Write) (stmt_list(stmt i))]
  [(eof) i]
  [else (error "Syntax error")]))

(define (stmt i)
  (case (first i)
  [(ID) (expr(match 'Eq (match 'ID i)))]
  [(Read) (match 'ID (match 'Read i))]
  [(Write) (expr(match 'Write i))]
  [else (error "Syntax error")]))

(define (expr i)
  (case (first i)
  [(ID Num Lparen) (term_tail(term i))]
  [else (error "Syntax error")]))

(define (term_tail i)
  (case (first i)
  [(Plus Minus) (term_tail(term(add_op i)))]
  [(Rparen ID Write Read eof) i]
  [else (error "Syntax error")]))

(define (term i)
  (case (first i)
  [(ID Num Lparen) (factor_tail(factor i))]
  [else (error "Syntax error")]))

(define (factor_tail i)
  (case (first i)
  [(Multiply Divide) (factor_tail(factor(mult_op i)))]
  [(Plus Minus Rparen ID Write Read eof) i]
  [else (error "Syntax error")]))

(define (factor i)
  (case (first i)
  [(ID) (match 'ID i)]
  [(Num) (match 'Num i)]
  [(Lparen) (match 'Rparen (expr(match 'Lparen i)))]
  [else (error "Syntax error")]))

(define (add_op i)
  (case (first i)
  [(Plus) (match 'Plus i)]
  [(Minus) (match 'Minus i)]
  [else (error "Syntax error")]))

(define (mult_op i)
  (case (first i)
  [(Multiply) (match 'Multiply i)]
  [(Divide) (match 'Divide i)]
  [else (error "Syntax error")]))

;(scanner input)
(define token-list(scanner input))
(program token-list)
;(trace scanner)