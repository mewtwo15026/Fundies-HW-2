;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Problem Set 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#| PROBLEM 1 |#

;; P.1.2
(define-struct lecture-hall (number capacity))
; A Lecture Hall is a structure:
;    (make-lecture-hall Number Number)
; interpr.: (make-lecture-hall n c) is the
; hall's room number and its maximum capacity

(define-struct automobile (year make model))
; An Automobile is a structure:
;    (make-automobile Number Symbol Symbol)
; interpr.: (make-automobile y mk md) is the
; year, manufacturer, and model of the car
; Constraints:
; - make and model must be paired
;   to form one of the following:
; MAKE        MODEL
; 'Toyota     'Corolla
; 'Subaru     'Forester
; 'Chevrolet  'Aveo

(define-struct football-player (name position number))
; A Football Player is a structure:
;    (make-football-player String Symbol Number)
; interpr.: (make-football-player na p nu) is the
; player's last name, position, and number
; Constraints:
; - number must be between 1 and 99
; - position is one of the following:
;   'QB 'WR 'RB 'TE 'K

(define-struct shirt (material size color))
; A Shirt is a structure:
;    (make-shirt Symbol Symbol Symbol)
; interpr.: (make-shirt m s c) is the shirt's
; material, size, and primary color
; Constraints:
; - material must be one of: 'cotton 'polyester 'wool
; - size must be one of: 'S 'M 'L 'XL
; - color must be one of: 'red 'blue 'green 'white 'black

; PROBLEM 1.3

; Lecture Hall -> ?
(define (lect-temp lh)
  ( ...(lecture-hall-number lh)...(lecture-hall-capacity lh)))

; Automobile -> ?
(define (auto-temp car)
  ( ...(automobile-year car)...(automobile-make car)
    ...(automobile-model car)))

; Football Player -> ?
(define (football-temp fbp)
  ( ...(football-player-name fbp)
    ...(football-player-position fbp)
    ...(football-player-number fbp)))

; Shirt -> ?
(define (shirt-temp s)
  ( ...(shirt-material s)...(shirt-size s)...(shirt-color s)))
#| PROBLEM 2 |#

(define-struct time (hours minutes))
; A Time is a structure:
;    (make-time Number Number)
; interpretation: (make-time h m) is the time  
; expressed in hours, and minutes
; Constraints:
; – hours is always between 0 and 11
; – minutes is always between 0 and 59

;; tock : Time -> Time
;; adds a minute to the given time
(define (tock tin)
  (cond [(= (time-minutes tin) 59)
         (cond [(= (time-hours tin) 11)
                (make-time 0 0)]
               [else (make-time (+ (time-hours tin) 1) 0)])]
        [else (make-time (time-hours tin) (+ (time-minutes) 1))]))

;; time->text : Time -> Image
;; convert time to a standard digital display

#| PROBLEM 3 |#