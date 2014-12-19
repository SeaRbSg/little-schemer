#lang racket

(require "../../lib/shared.rkt")
(require rackunit)

; based on exercism problem about rna transcription

; dna <-> rna
;   G <-> C
;   C <-> G
;   T <-> A
;   A <-> U

(define rna_match
  (lambda (nuc)
    (cond
      ((eq? nuc 'G) 'C)
      ((eq? nuc 'C) 'G)
      ((eq? nuc 'T) 'A)
      ((eq? nuc 'A) 'U)
      (else 'Alien_RNA))))

(define dna_match
  (lambda (nuc)
    (cond
      ((eq? nuc 'G) 'C)
      ((eq? nuc 'C) 'G)
      ((eq? nuc 'A) 'T)
      ((eq? nuc 'U) 'A)
      (else 'Alien_RNA))))

(define complement_of_dna
  (lambda (strand)
    (cond
      ((null? strand) strand)
      ((atom? strand) (rna_match strand))
      (else (cons (rna_match (car strand)) (complement_of_dna (cdr strand)))))))

(define complement_of_rna
  (lambda (strand)
    (cond
      ((null? strand) strand)
      ((atom? strand) (dna_match strand))
      (else (cons (dna_match (car strand)) (complement_of_rna (cdr strand)))))))

    
(module+ test
  (check-equal? (complement_of_dna 'C) 'G)
  (check-equal? (complement_of_dna 'G) 'C)
  (check-equal? (complement_of_dna 'T) 'A)
  (check-equal? (complement_of_dna 'A) 'U)
  (check-equal? (complement_of_dna '(A C G T G G T C T T A A )) '(U G C A C C A G A A U U))
  (check-equal? (complement_of_rna 'C) 'G)
  (check-equal? (complement_of_rna 'G) 'C)
  (check-equal? (complement_of_rna 'U) 'A)
  (check-equal? (complement_of_rna 'A) 'T)
  (check-equal? (complement_of_rna '(U G A A C C C G A C A U G)) '(A C T T G G G C T G T A C))
)
