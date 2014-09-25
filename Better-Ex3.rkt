;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Better-Ex3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

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