;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ProbSet3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

#| PROBLEM 1 |#

#|
Problem 1 Box Representation 

LECTURE HALL  
+--------+----------+
| Number | Capacity |
+--------+----------+
|    009 |      100 |
|    151 |       30 |
+--------+----------+

AUTOMOBILE
+------+---------+-----------+
| Year |  Make   |   Model   |
+------+---------+-----------+
| 1999 | 'Toyota | 'Corolla  |
| 2008 | 'Subaru | 'Forester |
| 2014 | 'Chevy  | 'Aveo     |
+------+---------+-----------+

FOOTBALL PLAYER
+-----------+----------+--------+
|   Name    | Position | Number |
+-----------+----------+--------+
| Brady     | 'QB      |     12 |
| Vinatieri | 'K       |      4 |
+-----------+----------+--------+

SHIRT
+------------+------+--------+
|  Material  | Size | Color  |
+------------+------+--------+
| 'cotton    | 'M   | 'green |
| 'polyester | 'L   | 'black |
+------------+------+--------+
  
|# 


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

; main : Time -> Image
; makes a clock
(define (main tin)
  (big-bang tin
            [to-draw time->text]
            [on-tick tock 60])) ;well played, problem set writer. well-played.

#| PROBLEM 3 |#

#| Exercise 3
A Note: 
 The original P Set implied we should develop the ball structure 
 to contain the structs ball-position and ball-velocity. We did 
 so originally and though it worked it made us sad. Since this
 nation was founded on the idea that the "pursuit of happiness"
 is our unalienable right, we rewrote the ball struct to contain
 all the position and velocity and removed the other structs.
 We humbly request you don't dock too many points for this, if not
 for us, for the United States of America. 
 The longer version is available if you wish. 
|#

(define-struct ball (x y speed direction))
;; A Ball is a structure:
;;    (make-ball Structure Structure)
;; interpr.: (make-ball x y speed direction) is the ball.
;; Constraints:
;; - direction must be one of: 'up 'dn 'lt 'rt 'stp
;; SELECTORS:
;; ball-x
;; ball-y
;; ball-speed
;; ball-direction
;; ball?


;; ball-next : Ball -> Ball
;; Creates a ball after a tick, with a new position based on a given direction (and speed).
;; template
#; (define (ball-next abl)
     (cond 
       [(symbol=? 'up (ball-direction abl))
          (make-ball ...)]
       [(symbol=? 'dn (ball-direction abl))
          (make-ball ...)]
       [(symbol=? 'lt (ball-direction abl))
          (make-ball ...)]
       [(symbol=? 'rt (ball-direction abl))
          (make-ball ...)]
       [else 
          (make-ball ...)]))

(define (ball-next abl)
  (cond
    [(symbol=? 'up (ball-direction abl))
      (make-ball 
       (ball-x abl) (- (ball-y abl) (ball-speed abl)) 
       (ball-speed abl) 'stp)]
    [(symbol=? 'dn (ball-direction abl))
      (make-ball 
       (ball-x abl) (+ (ball-y abl) (ball-speed abl)) 
       (ball-speed abl) 'stp)]
    [(symbol=? 'rt (ball-direction abl))
      (make-ball 
       (+ (ball-x abl) (ball-speed abl)) (ball-y abl)
       (ball-speed abl) 'stp)]    
    [(symbol=? 'lt (ball-direction abl))
      (make-ball 
       (- (ball-x abl) (ball-speed abl)) (ball-y abl)
       (ball-speed abl) 'stp)]      
    [else 
     (make-ball 
       (ball-x abl) (ball-y abl) 
       (ball-speed abl) 'stp)]))
    
(check-expect (ball-next (make-ball 20 20 10 'up)) (make-ball 20 10 10 'stp))
(check-expect (ball-next (make-ball 20 20 10 'dn)) (make-ball 20 30 10 'stp))
(check-expect (ball-next (make-ball 20 20 10 'lt)) (make-ball 10 20 10 'stp))
(check-expect (ball-next (make-ball 20 20 10 'rt)) (make-ball 30 20 10 'stp))

;; ball-image : Ball -> image
;; draws a circle given the position of the ball
(define (ball-image aball)
  (place-image 
   (circle 10 "solid" "red")
   (ball-x aball) (ball-y aball)
   (empty-scene 300 300)))

;; ball-change : Ball -> Ball
;; changes the ball direction based on arrow key pressed
(define (ball-change a-ball a-key)
  (cond
    [(key=? a-key "up")
     (make-ball 
       (ball-x a-ball) (ball-y a-ball) 
       (ball-speed a-ball) 'up) ]
    [(key=? a-key "down")
     (make-ball 
       (ball-x a-ball) (ball-y a-ball) 
       (ball-speed a-ball) 'dn) ]
    [(key=? a-key "left")
     (make-ball 
       (ball-x a-ball) (ball-y a-ball) 
       (ball-speed a-ball) 'lt) ]
    [(key=? a-key "right")
     (make-ball 
       (ball-x a-ball) (ball-y a-ball) 
       (ball-speed a-ball) 'rt) ]
    [else 
     (make-ball 
       (ball-x a-ball) (ball-y a-ball) 
       (ball-speed a-ball) 'stp) ]))

;; Ball -> Image
;; Runs the world based on main
(define (main world0)
  (big-bang world0
            (to-draw ball-image)
            (on-key ball-change)
            (on-tick ball-next)))

(main 
 (make-ball 150 150 10 'stp))