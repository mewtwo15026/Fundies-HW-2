;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Problem Set 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#| PROBLEM 1 |#

;; PROBLEM 1.1
; box reps - data, then examples

;; PROBLEMS 1.1 and 1.2
(define-struct lecture-hall (number capacity))
; A Lecture Hall is a structure:
;    (make-lecture-hall Number Number)
; interpr.: (make-lecture-hall n c) is the
; hall's room number and its maximum capacity
; SELECTORS:
; lecture-hall-number
; lecture-hall-capacity
; lecture-hall?

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
; SELECTORS:
; automobile-year
; automobile-make
; automobile-model
; automobile?

(define-struct football-player (name position number))
; A Football Player is a structure:
;    (make-football-player String Symbol Number)
; interpr.: (make-football-player na p nu) is the
; player's last name, position, and number
; Constraints:
; - number must be between 1 and 99
; - position is one of the following:
;   'QB 'WR 'RB 'TE 'K
; SELECTORS:
; football-player-name
; football-player-position
; football-player-number
; football-player?

(define-struct shirt (material size color))
; A Shirt is a structure:
;    (make-shirt Symbol Symbol Symbol)
; interpr.: (make-shirt m s c) is the shirt's
; material, size, and primary color
; Constraints:
; - material must be one of: 'cotton 'polyester 'wool
; - size must be one of: 'S 'M 'L 'XL
; - color must be one of: 'red 'blue 'green 'white 'black
; SELECTORS:
; shirt-material
; shirt-size
; shirt-color
; shirt?

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
(require 2htdp/image)
(require 2htdp/universe)

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
               [else (make-time (add1 (time-hours tin)) 0)])]
        [else (make-time (time-hours tin) (add1 (time-minutes tin)))]))

(check-expect (tock (make-time 4 20)) (make-time 4 21))
(check-expect (tock (make-time 4 59)) (make-time 5 0))
(check-expect (tock (make-time 11 59)) (make-time 0 0))

;; time->text : Time -> Image
;; convert time to a standard digital display
(define (time->text tin)
   (text (string-append (cond [(= (time-hours tin) 0) "12"]
                              [(< (time-hours tin) 10)
                               (string-append "0"
                                              (number->string (time-hours tin)))]
                              [else (number->string (time-hours tin))]) 
                        ":" 
                        (cond [(< (time-minutes tin) 10)
                               (string-append "0"
                                              (number->string (time-minutes tin)))]
                              [else (number->string (time-minutes tin))])) 
         24 "olive"))

(check-expect (time->text (make-time 4 20))
              (text "04:20" 24 'olive))
(check-expect (time->text (make-time 11 7))
              (text "11:07" 24 'olive))

(define (main tin)
  (big-bang tin
            [to-draw time->text]
            [on-tick tock])) ;well played, problem set writer. well-played.

#| PROBLEM 3 |#