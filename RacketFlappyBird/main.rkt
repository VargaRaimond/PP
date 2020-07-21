#lang racket/gui

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "abilities.rkt")
(require "constants.rkt")
(require "random.rkt")

;---------------------------------------checker_exports------------------------------------------------
(provide next-state)
(provide next-state-bird)
(provide next-state-bird-onspace)
(provide change)

(provide get-pipes)
(provide get-pipe-x)
(provide next-state-pipes)
(provide add-more-pipes)
(provide clean-pipes)
(provide move-pipes)

(provide invalid-state?)
(provide check-ground-collision)
(provide check-pipe-collisions)

(provide draw-frame)

(provide get-initial-state)
(provide get-bird)
(provide get-bird-y)
(provide get-bird-v-y)

; pipe
(provide get-pipes)
(provide get-pipe-x)

; score
(provide get-score)

(provide get-abilities)
(provide get-abilities-visible)
(provide get-abilities-active)
; variables
(provide get-variables)
(provide get-variables-gravity)
(provide get-variables-momentum)
(provide get-variables-scroll-speed)

;---------------------------------------checker_exports------------------------------------------------
;Initial state
; Primul pas pe care trebuie sa il facem este sa cream starea initiala a jocului.
; Aceasta va fi salvata in (get-initial-state), si trebuie sa incapsuleze toate informatiile
; necesare jocului, si anume: informatii despre pasare, despre pipes si despre powerups.
; Recomandam ca in pasare, sa retineti, printre altele, informatii despre y-ul curent
; si viteza pe y
; Pe parcursul temei, in state, salvati coordonatele colturilor din stanga sus ale obiectelor.
; Aceasta va face mai usoara atat logica miscarii obiectelor, cat si testarea cerintelor.
; Toate coordonatele oferite in comentarii sau in fisierul constants.rkt, se refera la
; coltul din stanga sus ale obiectelor!
;Inițial state
; Primul pas pe care trebuie să îl facem este să creăm starea inițială a jocului.
; Aceasta va fi salvată în (get-initial-state), și trebuie să incapsuleze toate informațiile
; necesare jocului, și anume: informații despre pasăre, despre pipes și, pentru bonus,
; despre powerups și despre variabilele de mediu.
; Recomandăm ca în pasăre, să rețineți, printre altele, informații despre y-ul curent
; și viteză pe y.
; Pe parcursul temei, în state, salvați coordonatele colțurilor din stânga sus ale obiectelor.
; Aceasta va face mai ușoară atât logică mișcării obiectelor, cât și testarea cerințelor.
; Toate coordonatele oferite în comentarii sau în fișierul variables.rkt se referă la
; colțul din stânga sus ale obiectelor!

; define variables structure
(define-struct my-variables (gravity momentum scroll-speed))
(define initial-variables (my-variables initial-gravity initial-momentum initial-scroll-speed))

; define a bird structure and an instance named b
(define-struct my_bird (x y v-y))

; define state structure and instantiate in initial-state
(define-struct game-state (bird pipes score variables abilities))

; create pipes
(define-struct my_pipe (x y-gap))

; prepare initial state with all instances
; return initial state
(define (get-initial-state)
  (game-state (my_bird bird-x bird-initial-y 0)
              (cons (my_pipe scene-width (+ added-number (random random-threshold))) null)
              0
              initial-variables
              (list null null)))
;(fill-abilities null DISPLAYED_ABILITIES ABILITIES)
; getters for bird, bird y and bird speed
(define (get-bird state)
  (game-state-bird state))

(define (get-bird-y bird)
  (my_bird-y bird))

(define (get-bird-v-y bird)
  (my_bird-v-y bird))

; update bird speed with gravity move bird with v-y speed
(define (next-state-bird bird gravity)
  (struct-copy my_bird bird [v-y (+ (get-bird-v-y bird) gravity)]
               [y (+ (get-bird-y bird) (get-bird-v-y bird))]))

; move bird up reseting speed
(define (next-state-bird-onspace bird momentum)
  (struct-copy my_bird bird [v-y (- 0 momentum)]))

; update bird's momentum but we need to return a state so we copy the new bird in current state
(define (change current-state pressed-key)
    (if (key=? pressed-key " ")
        (struct-copy game-state current-state
                     [bird (next-state-bird-onspace (get-bird current-state) initial-momentum)])
         current-state))

; getters for pipes and one pipe's x position
(define (get-pipes state)
  (game-state-pipes state))

(define(get-pipe-x pipe)
  (my_pipe-x pipe))

; move all pipes using helper function
(define (move-pipes pipes scroll-speed)
  (map (λ (x) (update-pipe-x x scroll-speed)) pipes
   ))
  
; helper function that moves one pipe
(define (update-pipe-x pipe scroll-speed)
  (struct-copy my_pipe pipe [x (- (get-pipe-x pipe) scroll-speed)]))

; filter the pipes that reached the end of the scene
(define (clean-pipes pipes)
  (filter (λ (pipe) (> (+ (my_pipe-x pipe) pipe-width) 0)) pipes))

; add pipes when the others are deleted
(define (add-more-pipes pipes)
  (if (< (length pipes) no-pipes)
      (cons (my_pipe (+ pipe-width pipe-gap (my_pipe-x (car pipes)) bird-width)
                     (+ added-number (random random-threshold))) pipes)
      pipes))


; apply all pipe functions to update them every frame
(define (next-state-pipes pipes scroll-speed)
  (add-more-pipes (clean-pipes (move-pipes pipes scroll-speed))))

; score getter
(define (get-score state)
  (game-state-score state))

; check if the bird touches the ground
(define (check-ground-collision bird)
 (>= (+ (get-bird-y bird) bird-height) ground-y))

; stop the game if the bird touches a pipe or the ground
(define (invalid-state? state)
  (or (check-ground-collision (game-state-bird state))
      (check-pipe-collisions (get-bird state) (get-pipes state))))

; check every pipe until we finish them or find a collision
(define (check-pipe-collisions bird pipes)
  (cond [(null? pipes) #f]
        [(check-one-pipe-collision bird (car pipes)) #t]
        [else (check-pipe-collisions bird (cdr pipes))]))

; calculate all points so we can call the given function for both
; pipe parts
(define (check-one-pipe-collision bird pipe)
  (match-let ([(my_bird x y _) bird] [(my_pipe x-p y-gap) pipe])
     (let ([bird-top (make-posn x y)]
           [bird-bot (make-posn (+ x bird-width) (+ y bird-height))]
           [pipe-up-top (make-posn x-p 0)]
           [pipe-up-bot (make-posn (+ x-p pipe-width) y-gap)]
           [pipe-down-top (make-posn x-p (+ y-gap pipe-self-gap))]
           [pipe-down-bot (make-posn (+ x-p pipe-width) scene-height)])
       (or (check-collision-rectangles bird-top bird-bot pipe-up-top pipe-up-bot)
           (check-collision-rectangles bird-top bird-bot pipe-down-top pipe-down-bot))
       )))

(define (check-collision-rectangles A1 A2 B1 B2)
  (match-let ([(posn AX1 AY1) A1]
              [(posn AX2 AY2) A2]
              [(posn BX1 BY1) B1]
              [(posn BX2 BY2) B2])
    (and (< AX1 BX2) (> AX2 BX1) (< AY1 BY2) (> AY2 BY1))))

; update the score every frame
(define (next-state-score current-score)
  (+ current-score 0.1))
  ;(struct-copy score current-score
               ;[current (+ (score-current current-score) (score-inc current-score))]))

; update all components of current state using all next-state functions
(define (next-state state)
  (struct-copy game-state state
               [bird (next-state-bird (get-bird state) initial-gravity)]
               [pipes (next-state-pipes (get-pipes state) (get-variables-scroll-speed (get-variables state)))]
               [score (next-state-score (get-score state))]
               [variables (next-variables (get-variables state) (get-abilities state))]
               [abilities (next-abilities (get-abilities state) (get-bird state) (get-variables-scroll-speed (get-variables state)))]
               ))


; all images defines
(define bird (rectangle bird-width bird-height  "solid" "yellow"))
(define ground (rectangle scene-width ground-height "solid" "brown"))
(define initial-scene (rectangle scene-width scene-height "solid" "white"))

(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))
(define (score-to-image x)
(if SHOW_SCORE
	(apply text/font (~v (round x)) 24 "indigo" text-family)
	empty-image))

; draw bird->ground->score->active_abilities->pipes->visible_abilities->empty_scene
; abilties double list parameter
(define (draw-frame state)
  (match-let ([(my_bird x y v-y) (get-bird state)])
    (place-images (list bird ground)
                  (list
                   (center->corner x y bird-width bird-height)
                   (center->corner 0 ground-y scene-width ground-height))
                  (place-score (get-score state)
                               (place-active-abilities (get-abilities state)
                                                       (place-pipes (get-pipes state)
                                                                    (place-visible-abilities (get-abilities state)
                                                                                             initial-scene)
                                                                    ))))))
; draw score in front of the scene
(define (place-score current-score scene)
  (place-image (score-to-image current-score) text-x text-y scene))

; draw all pipes
(define (place-pipes pipes scene)
  (foldl (λ (pipes-var scene)(match-let ([(my_pipe x y-gap) pipes-var])
    (place-images (list (rectangle pipe-width y-gap "solid" "green")
                        (rectangle pipe-width (if (> (- pipe-height y-gap pipe-self-gap) 0)
                                               (- pipe-height y-gap pipe-self-gap) 0) "solid" "green"))
                  (list (center->corner x 0 pipe-width y-gap)
                        (center->corner x (+ y-gap pipe-self-gap) pipe-width (- pipe-height y-gap pipe-self-gap)))
                  scene)
         )) scene pipes))

; returns new coordinates for drawing
(define (center->corner x y width height)
  (make-posn (+ x (quotient width 2)) (+ y (quotient height 2))))

; Bonus
; Completați abilities.txt mai întâi, aceste funcții căt, apoi legați
; această funcționalitate la jocul inițial.


; Abilitatea care va accelera timpul va dura 10 de secunde, va avea imaginea (hourglass "tomato")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = scroll-speed + 1
(define fast-ability (my-ability (hourglass "mediumseagreen") 10 null
                                 (λ (variables) (struct-copy my-variables variables
                                                             [scroll-speed (add1 (get-variables-scroll-speed variables))]))))

; Abilitatea care va încetini timpul va dura 30 de secunde, va avea imaginea (hourglass "mediumseagreen")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = max(5, scroll-speed - 1)
(define slow-ability (my-ability (hourglass "tomato") 30 null
                                 (λ (variables) (struct-copy my-variables variables
                                                             [scroll-speed (sub1 (get-variables-scroll-speed variables))]))))

; lista cu toate abilităţile posibile în joc
(define ABILITIES (list slow-ability fast-ability))

(define (get-variables state)
  (game-state-variables state))
(define (get-variables-gravity variables)
  (my-variables-gravity variables))
(define (get-variables-momentum variables)
  (my-variables-momentum variables))
(define (get-variables-scroll-speed variables)
  (my-variables-scroll-speed variables))


; return already built double list
(define (get-abilities x) (game-state-abilities x))

; first list represents visible abilities
(define (get-abilities-visible x) (car x))

; second list represents active abilities
(define (get-abilities-active x) (cadr x))

; filter visible abilities by checking if they got out of screen
; abilties visible parameter
(define (clean-abilities abilities)
	(filter (λ (ability)
                  (> (+ (posn-x (get-ability-pos ability)) (/ (image-width (get-ability-image ability)) 2)) 0))
                abilities))


; move abilities on screen with current scroll-speed
; abilties visible parameter
(define (move-abilities abilities scroll-speed)
  (map (λ (ability)
         (let ([ability-pos (get-ability-pos ability)])
         (set-ability-pos ability (- (posn-x ability-pos) scroll-speed) (posn-y ability-pos)))) abilities))


; decrease active abilities time with 1 every second until we filter it out
; because it reached 0 or lower
; abilities active parameter
(define (time-counter abilities)
  (filter-not (λ (ability)  (<= (get-ability-time ability) 0))
          (map (λ (x) (set-ability-time x (- (get-ability-time x) (/ 1 fps)))) abilities)))


; apply already defined functions to calculate the visible abilities
; for the next state
; abilities visible parameter
(define (next-abilities-visible visible scroll-speed)
	(fill-abilities (clean-abilities (move-abilities visible scroll-speed)) DISPLAYED_ABILITIES ABILITIES))


; regenerate abilities double list:
; - for the first list we get all visible abilities and filter out collisions then prepare them for next state
; - for the second we filter new collisions and add them at the end of the current active abilities
; to which we decrease time
; abilities double list parameter
(define (next-abilities abilities bird scroll-speed)
  (list (next-abilities-visible (filter-not (λ  (abil) (ability-collision bird abil)) (car abilities)) scroll-speed)
        (append (time-counter (cadr abilities))
                (filter (λ (abil) (ability-collision bird abil))
                           (car abilities)))))

; test bird-ability collision using the same function we used for pipes
; and a let* to extract all coordinates
(define (ability-collision bird ability)
  (let* ([bird-top (make-posn (my_bird-x bird) (my_bird-y bird))]
        [bird-bot (make-posn (+ (my_bird-x bird) bird-width) (+ (my_bird-y bird) bird-height))]
        [ab-pos (get-ability-pos ability)]
        [wdth (image-width (get-ability-image ability))]
        [hgth (image-height (get-ability-image ability))]
        [ability-top (make-posn (- (posn-x ab-pos) (/ wdth 2)) (- (posn-y ab-pos) (/ hgth 2)) )]
        [ability-bot (make-posn (+ (posn-x ab-pos) (/ wdth 2)) (+ (posn-y ab-pos) (/ hgth 2)) )])
    (check-collision-rectangles bird-top bird-bot ability-top ability-bot)
    ))

; calculate next-state variables by applying all speed bufs or nerfs from current active abilities
; abilities double list parameter
(define (next-variables variables abilities)
 ((compose-abilities (map get-ability-next (get-abilities-active abilities))) initial-variables))



; place visible abilities on screen
; abilities double list parameter
(define (place-visible-abilities abilities scene)
	(place-images (map get-ability-image (get-abilities-visible abilities))
                      (map get-ability-pos (get-abilities-visible abilities)) scene))

; place current active abilities on screen after we scale them to 0.75
; use a named let to increase the offset so we don't place them one over the other
; abilities double list parameter
(define (place-active-abilities abilities scene)
  (let loop ([i 0] [all-abilities (get-abilities-active abilities)] [current-scene scene])
    (if (null? all-abilities) current-scene
        (loop (add1 i) (cdr all-abilities)
               (place-image (scale 0.75 (get-ability-image (car all-abilities)))
                            (- (posn-x abilities-posn) (* 50 i)) (posn-y abilities-posn) current-scene)
              ))))

(module+ main
	(big-bang (get-initial-state)
	 [on-tick next-state (/ 1.0 fps)]
	 [to-draw draw-frame]
	 [on-key change]
	 [stop-when invalid-state?]
	 [close-on-stop #t]
	 [record? #f]))
