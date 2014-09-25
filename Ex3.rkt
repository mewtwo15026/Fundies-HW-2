;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

#| Develop a data representation for the current position of the ball.
 The position is best described with a pair of positive integers. |#

(define-struct ballp (x y))
; A Ballp is a Posn:
;    (make-ballp Number Number)
; interpr.: (make-ballp x y) is the
; x and y position of the ball 
; SELECTORS:
; ballp-xcord
; ballp-ycord
; ballp?

#| Develop a data representation for the speed and direction of the ball.
You may assume that the ball always moves exactly 10 pixels at a time.|#

(define-struct ballv (speed direction))
; A Ballv is a structure:
;    (make-shirt Number Symbol)
; interpr.: (make-ballv s d) is the ball's
; Speed and Direction. In other words, its velocity
; Constraints:
; - direction must be one of: 'up 'dn 'lt 'rt 'stp
; SELECTORS:
; ballv-speed
; ballv-direction
; ballv?


#| Develop a data representation for the ball. |#
(define-struct ball (posn velocity))
; A Ball is a structure:
;    (make-ball Structure Structure)
; interpr.: (make-ball p v) is the ball.
; SELECTORS:
; ball-posn
; ball-velocity
; ball?

#| Design the function ball-next, which consumes the representation of a
 ball and creates a ball that represents where it will be after one tick. |#

;; ball-next : Ball -> Ball
;; Creates a ball after a tick
(define (ball-next abl)
  (cond 
    [ (symbol=? 'dn (ballv-direction (ball-velocity abl)))
      (make-ball 
       (make-ballp (ballp-x (ball-posn abl)) (+ (ballp-y (ball-posn abl)) (ballv-speed (ball-velocity abl)))) 
       (make-ballv (ballv-speed (ball-velocity abl)) 'stp)) ]
    [ (symbol=? 'rt (ballv-direction (ball-velocity abl)))
      (make-ball 
       (make-ballp (+ (ballp-x (ball-posn abl)) (ballv-speed (ball-velocity abl))) (ballp-y (ball-posn abl))) 
       (make-ballv (ballv-speed (ball-velocity abl)) 'stp)) ]
    [ (symbol=? 'lt (ballv-direction (ball-velocity abl)))
      (make-ball 
       (make-ballp (- (ballp-x (ball-posn abl)) (ballv-speed (ball-velocity abl))) (ballp-y (ball-posn abl))) 
       (make-ballv (ballv-speed (ball-velocity abl)) 'stp)) ]
    [ (symbol=? 'up (ballv-direction (ball-velocity abl)))
      (make-ball 
       (make-ballp (ballp-x (ball-posn abl)) (- (ballp-y (ball-posn abl)) (ballv-speed (ball-velocity abl)) )) 
       (make-ballv (ballv-speed (ball-velocity abl)) 'stp)) ]
    [else 
     (make-ball 
       (make-ballp (ballp-x (ball-posn abl)) (ballp-y (ball-posn abl)))
       (make-ballv (ballv-speed (ball-velocity abl)) (ballv-direction (ball-velocity abl)))) ]))
    

;(check-expect (tock (make-time 4 20)) (make-time 4 21))
;(check-expect (tock (make-time 4 59)) (make-time 5 0))


#| Design the function ball-image, which consumes representation of a ball
and produces a rectangle of 300x300 pixels with a red dot (diameter 10 pixels) 
placed at the ball’s position. |#

;; ball-image : Ball -> image
;; draws a circle with a the position of the ball
(define (ball-image aball)
  (place-image 
   (circle 10 "solid" "red")
   (ballp-x (ball-posn aball)) (ballp-y (ball-posn aball))
   (empty-scene 300 300)))

#| Design the function ball-change, which consumes a ball and a key-event which 
represents the user hitting a key on the keyboard. If the key event represents 
the user hitting the up-arrow key, then ball-change produces a new ball which 
is just like the input ball, except that the new ball is moving upward; similarly 
for key events representing the left-, right- and down-arrow keys: they cause 
the resulting ball to be one that is moving left, right, or down, respectively. 
Passing any other keystroke to ball-change causes the ball to be unchanged. 
Reading about key-events in the HelpDesk is a good place to start. |#


;; ball-change : Ball -> Ball
;; changes the ball direction based on arrow key pressed
(define (ball-change a-ball a-key)
  (cond
    [(key=? a-key "left")
     (make-ball 
       (make-ballp (ballp-x (ball-posn a-ball)) (ballp-y (ball-posn a-ball)))
       (make-ballv (ballv-speed (ball-velocity a-ball)) 'lt)) ]
    [(key=? a-key "right")
     (make-ball 
       (make-ballp (ballp-x (ball-posn a-ball)) (ballp-y (ball-posn a-ball))) 
       (make-ballv (ballv-speed (ball-velocity a-ball)) 'rt)) ]
    [(key=? a-key "up")
     (make-ball 
       (make-ballp (ballp-x (ball-posn a-ball)) (ballp-y (ball-posn a-ball)))
       (make-ballv (ballv-speed (ball-velocity a-ball)) 'up)) ]
    [(key=? a-key "down")
     (make-ball 
       (make-ballp (ballp-x (ball-posn a-ball)) (ballp-y (ball-posn a-ball)))
       (make-ballv (ballv-speed (ball-velocity a-ball)) 'dn)) ]
    [else 
     (make-ball 
       (make-ballp (ballp-x (ball-posn a-ball)) (ballp-y (ball-posn a-ball))) 
       (make-ballv (ballv-speed (ball-velocity a-ball)) 'stp)) ]))

; Ball -> Ball
(define (main world0)
  (big-bang world0
            (to-draw ball-image)
            (on-key ball-change)
            (on-tick ball-next)))

(main  (make-ball 
       (make-ballp 150 150)
       (make-ballv 10 'stp)))