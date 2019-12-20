#lang racket

(define (prompt/read message) ; displays a prompt, then returns user input
  (if (display message) ; display return value is undefined, so either way this will display and read
      (read)
      (read)
      )
  ) ; this is odd. I'm not a huge fan of the method but I didn't want to use (begin) so I could stay within functional programming practice

(define (~= x y) ; checks if x is approximately equal to y
  (if (and (< (- x 1e-13) y) (> (+ x 1e-13) y))
      #t
      #f
      )
  )
(define (=* b x y) ; checks if x is within +/- b of y
  (if (and (< (- x b) y) (> (+ x b) y))
      #t
      #f
      )
  )

(define (in-function x y funcs symbols)
  (if (null? funcs)
      " "
      (if (and (=* 0.7 (real-part (with-handlers ((exn:fail:contract:divide-by-zero? ; catches division by zero and returns 1e13 as a substitute 
                   (lambda (exn) 1e13))) ((car funcs) x))) y) (real? ((car funcs) x)))
          (car symbols)
          (in-function x y (cdr funcs) (cdr symbols))
          )
      )
  )

; offset is used to ignore a number of spots so that the numbers stay lined up
(define (x-axis-numbers x x-max x-scale offset)
  (if (> x x-max)
      ""
      (if (> offset 0)
          (x-axis-numbers (+ x x-scale) x-max x-scale (- offset 1))
          (if (~= x (round x))
              (if (negative? (round x))
                       (string-append (~r x #:precision 0) (x-axis-numbers (+ x x-scale) x-max x-scale 1))
                       (string-append (~r x #:precision 0) (x-axis-numbers (+ x x-scale) x-max x-scale 0))
                  )
              (if (negative? (round x))
                  (if (~= (sin x) 0) ; if x is a factor of pi.. pretty sneaky eh? because modulo only takes integers
                      (string-append (~r (round (/ x pi))  #:precision 0) (string-append "pi" (x-axis-numbers (+ x x-scale) x-max x-scale 3)))
                      (string-append " " (x-axis-numbers (+ x x-scale) x-max x-scale 0))
                      )
                  (if (~= (sin x) 0)
                      (string-append (~r (round (/ x pi))  #:precision 0) (string-append "pi" (x-axis-numbers (+ x x-scale) x-max x-scale 2)))
                      (string-append " " (x-axis-numbers (+ x x-scale) x-max x-scale 0))
                      )
                  )
              )
          )
      )
  )

; prints out each space for x in a line
(define (x-in-line funcs x x-max y xscale yscale symbols)
  (if (> x x-max)
      (string-append (~r (/ y yscale) #:precision 2) "\n")
      (cond ((not (string=? (in-function x y funcs symbols) " ")) ; will not try to display imaginary numbers
             (string-append (in-function x y funcs symbols) (x-in-line funcs (+ x xscale) x-max y xscale yscale symbols)))
            ((~= x 0)
             (string-append "|" (x-in-line funcs (+ x xscale) x-max y xscale yscale symbols)))
            (else (if (= y 0)
                      (string-append "-" (x-in-line funcs (+ x xscale) x-max y xscale yscale symbols))
                      (string-append " " (x-in-line funcs (+ x xscale) x-max y xscale yscale symbols))
                      )
                  )
            )
      )
  )

; prints out each line using x-in-line
(define (line funcs x-begin x-max y y-min x-scale y-scale symbols)
  (if (< y y-min)
      (x-axis-numbers x-begin x-max x-scale 0)
      (string-append
       (x-in-line funcs x-begin x-max y x-scale y-scale symbols)
       (line funcs x-begin x-max (- y 1) y-min x-scale y-scale symbols)
       )
      )
  )

; returns list of augmented functions. all functions are edited such that essentailly the amplitude is increased
(define (augment-functions funcs y-min y-max)
  (if (null? funcs)
      '()
      (cons (lambda (x) (* ((car funcs) x) (/ 30 (- y-max y-min)))) (augment-functions (cdr funcs) y-min y-max))
      )
  )

; shows all the symbols and prompts for an input, returns that input
(define (pick-symbol* symbols number string) 
  (if (null? (cdr symbols))
      (prompt/read (string-append* string (list (number->string number) ": " (car symbols) "\n" "Input a number to choose a symbol: ")))
      (pick-symbol* (cdr symbols) (+ number 1) (string-append* string (list (number->string number) ": " (car symbols) "\n"))) 
      )
  )
; effectuated by pick-symbol*, becuase the arguments are messy
(define (pick-symbol symbols) 
  (pick-symbol* symbols 0 "")
  )

; returns a list without the ith element, starting from 0
(define (remove-element funcs number)
    (if (null? (cdr funcs))
        '()
        (if (= number 0)
            (if (list? (cdr funcs))
                (cdr funcs)
                (list (cdr funcs))
                )
            (cons (car funcs) (remove-element (cdr funcs) (- number 1)))
            )
        )
  )

; bulk of the IO happens through this function and the sub-functions.
(define (graph funcs x-begin x-max y-min y-max symbols)
  ; add a function with new character for displaying
  (define (add-function)
    (graph (cons (eval (prompt/read "Enter a lambda expression: ") ns) funcs) x-begin x-max y-min y-max (cons (symbol->string (prompt/read "Enter a new symbol: ")) symbols))
    )
  ; graphs without function specified by number
  (define (rem-function number) 
    (graph (remove-element funcs number) x-begin x-max y-min y-max (remove-element symbols number))
    )
  ; queries the user for changes to the bounds of the graph and calls graph with these new values
  (define (change-bounds)
    (graph funcs
           (eval (prompt/read "Enter lower x bound: ") ns)
           (eval (prompt/read "Enter upper x bound: ") ns)
           (eval (prompt/read "Enter lower y bound: ") ns)
           (eval (prompt/read "Enter upper y bound: ") ns)
           symbols)
    )
  ; main control centre for user input
  (define (parse-input input)
    (cond ((equal? input 'r) (rem-function (pick-symbol symbols)))
          ((equal? input 'a) (add-function))
          ((equal? input 'p) (display (string-append (line (augment-functions funcs y-min y-max) x-begin x-max (* y-max (/ 30 (- y-max y-min))) (* y-min (/ 30 (- y-max y-min))) (/ (- x-max x-begin) 100) (/ 30 (- y-max y-min)) symbols) "\n"))
                             (graph funcs x-begin x-max y-min y-max symbols))
          ((equal? input 'c) (change-bounds))
          ((equal? input 'q) (display "quit\n"))
          ((eof-object? input) (display "eof encountered\n"))
          (else (display "invalid input\n") (graph funcs x-begin x-max y-min y-max symbols))
          )
    )
  ; the actual body of the graph function
  (parse-input (prompt/read "What would you like to do? \nr: remove function \na: add function \np: print graph \nc: change bounds \nq: quit\n"))
  )
  
; define the namespace so eval can operate in a given namespace
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

; get the initial information
(define (input-graph)
  (graph (list (eval (prompt/read "Enter a lambda expression: ") ns))
         (eval (prompt/read "Enter lower x bound: ") ns)
         (eval (prompt/read "Enter upper x bound: ") ns)
         (eval (prompt/read "Enter lower y bound: ") ns)
         (eval (prompt/read "Enter upper y bound: ") ns)
         (list "*"))
  )
; finally, start the program
(input-graph)



