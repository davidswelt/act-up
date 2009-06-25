;; ACT-UP Version of the BST LEARN MODEL (sticks model)
;; David Reitter 06/2009 reitter@cmu.edu

(require "act-up" "act-up.lisp")


(defrule decide ()
  (cond 
    ((> (- under over) 25)  ;;  same as (< over (- under 25))
     (decide-over))
    ((> (- over under) 25)
     (decide-under))
    (t  ;; let utility decide
     (choose-strategy))))


(defrule  decide-over ()
  :group choose-strategy
  (visual-move-attention :kind oval :screen-y 60)
  (move-mouse (make-location 60 0))
)
(defrule decide-under ()
  :group choose-strategy
  (visual-move-attention :kind oval :screen-y 85)
  (move-mouse (make-location 85 0))
)


(defrule move-mouse (location)
  (manual-move-cursor :loc location)
  (manual-click-mouse))

