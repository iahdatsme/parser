#lang racket
(require racket/trace)

;; SCANNER
(define input (string->list (file->string "input01.txt")))

(define (scanner i)
  (cond
    [(or(empty? i) (equal? (first (list i)) #\$)) (list 'eof)]
    [(char-numeric? (first i)) (cons 'Num (scanner (rest i)))]
    [(char-alphabetic? (first i)) (cons 'ID (scanner (rest i)))]
    [(or (equal? (first i) #\space) (equal? (first i) #\return) (equal? (first i) #\newline)) (scanner (rest i))]
    [(and (equal? (first i) #\:) (equal? (second i) #\=)) (cons 'Eq (scanner (rest (rest i))))]
    [(equal? (first i) #\() (cons 'Lparen (scanner (rest i)))]
    [(equal? (first i) #\)) (cons 'Rparen (scanner (rest i)))]
    [(equal? (first i) #\+) (cons 'Plus (scanner (rest i)))]
    [(equal? (first i) #\*) (cons 'Multiply (scanner (rest i)))]
    [(equal? (first i) #\-) (cons 'Minus (scanner (rest i)))]
    [(equal? (first i) #\/) (cons 'Divide (scanner (rest i)))]
    

    [else "Scanner Error" (rest i)]
    )
  
  )

(scanner input)
(trace scanner)