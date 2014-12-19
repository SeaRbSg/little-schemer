#lang racket

(require "../../lib/shared.rkt")
(require rackunit)
(require lang/htdp-advanced)

; based on exercism problem about bob the lackadaisical teenager
; I took out special chars like ,.'
; I am forced to separate the ? from the last word because I dont know how to parse a string in scheme

(define last_char
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((null? (cdr lat)) (car lat))
      (else (last_char (cdr lat))))))

(module+ test
  (check-equal? (last_char '(1 2 3)) 3)
  (check-equal? (last_char '(mi carro me lo robaron ahiii)) 'ahiii)
  (check-equal? (last_char '()) '()))

; With Bob I have met my match. There is no way to reason with a lackadaisical teenager. 
; Besides, I have no idea how to operate with strings. I'll leave it for later
(define all_caps?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else 
        (cond
          ((eq? (make-string (car lat)) (string-upper-case? (stringify_you_bastard! (car lat)))) (all_caps? (cdr lat)))
          (else #f))))))

(module+ test
  (check-true (all_caps? '(PEPE JUAN)))
)

    
(define bob_hey
  (lambda (low) ; low is list of words
    (cond
      ((eq? (last_char low) '?) '(Sure))
      ((all_caps? low) '(Whoa chill out!))
      (else '(Whatever)))))

(module+ test
  (check-equal? (bob_hey '(Tom-ay-to tom-aaaah-to)) '(Whatever))
  (check-equal? (bob_hey '(WATCH OUT!)) '(Whoa chill out!))
  ;(check-equal? (bob_hey '(Does this cryogenic chamber make me look fat ?)) '(Sure))
  ;(check-equal? (bob_hey '(Lets go make out behind the gym!)) '(Whatever))
  ;(check-equal? (bob_hey '(Its OK if you dont want to go to the DMV)) '(Whatever))
  ;(check-equal? (bob_hey '(WHAT THE HELL WERE YOU THINKING ?)) '(Whoa chill out!))
  ;(check-equal? (bob_hey '(1 2 3 GO!)) '(Whoa chill out!))
  ;(check-equal? (bob_hey '(1 2 3)) '(Whatever))
  ;(check-equal? (bob_hey '(ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!)) '(Whoa chill out!)))
  ;(check-equal? (bob_hey '(I HATE YOU)) '(Whoa chill out!))
  ;(check-equal? (bob_hey '(Ending with a ? means a question)) '(Whatever))
  ;(check-equal? (bob_hey '(Wait! Hang on  Are you going to be OK?)) '(Sure))
  ;(check-equal? (bob_hey '()) '(Fine Be that way!))
)
