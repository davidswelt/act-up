;; ACT-UP Version of the BST LEARN MODEL (sticks model)
;; David Reitter 06/2009 reitter@cmu.edu

(require "act-up" "../act-up.lisp")


(defproc decide ()
  (cond 
    ((> (- under over) 25)  ;;  same as (< over (- under 25))
     (decide-over))
    ((> (- over under) 25)
     (decide-under))
    (t  ;; let utility decide
     (choose-strategy))))


(defproc  decide-over ()
  :group choose-strategy
  (visual-move-attention :kind oval :screen-y 60)
  (move-mouse (make-location 60 0))
)
(defproc decide-under ()
  :group choose-strategy
  (visual-move-attention :kind oval :screen-y 85)
  (move-mouse (make-location 85 0))
)


(defproc move-mouse (location)
  (manual-move-cursor :loc location)
  (manual-click-mouse))

