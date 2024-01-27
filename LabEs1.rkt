;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname LabEs1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define frase ; val: stringa
  (lambda (sogg pv co) ; sogg, pv, co stringhe
    (string-append (adatta-parola sogg) " " (if (isPlurale sogg) (adatta-verbo-plurale pv) (adatta-verbo-singolare pv)) " " (adatta-parola co))
 ))

(define isPlurale
  (lambda (parola)
     (let (
            (ultima-lettera (substring parola (- (string-length parola) 1)))
            )
    (cond
      ((string=? ultima-lettera "e")
       true)
      ((string=? ultima-lettera "i")
       true)
      (else false)
     )
      )
  ))

(define adatta-parola
  (lambda(parola)
    (let (
          (ultima-lettera (substring parola (- (string-length parola) 1)))
          )
    (cond
      ( (string=? ultima-lettera "a" )
       (femminile parola) )

      ( (string=? ultima-lettera "e")
       (femminile parola) )
      ( (string=? ultima-lettera "o")
       (maschile parola) )
      ( (string=? ultima-lettera "i")
       (maschile parola) )
     )
      )
  ))

(define femminile
  (lambda (parola)
    (if (string=? (substring parola (- (string-length parola) 1)) "a")
        (string-append "la " parola)
        (string-append "le " parola)
     )
  ))

(define maschile
  (lambda (parola)
    (if (string=? (substring parola (- (string-length parola) 1)) "o")
        (string-append "il " parola)
        (string-append "i " parola)
     )
  ))

(define adatta-verbo-plurale
  (lambda (verbo)
    (let (
          (ultime-3-lettere (substring verbo 0 (- (string-length verbo) 3)))
          )
    (if (string=? (substring verbo (- (string-length verbo) 3)) "are")
     (string-append ultime-3-lettere "ano") 
     (string-append ultime-3-lettere "ono")
    )
      )
 ))
 
(define adatta-verbo-singolare
  (lambda (verbo)
    (let (
          (ultime-3-lettere (substring verbo 0 (- (string-length verbo) 3)))
          )
    (if (string=? (substring verbo (- (string-length verbo) 3)) "are")
     (string-append ultime-3-lettere "a") 
     (string-append ultime-3-lettere "e")
    )
      )
 ))

