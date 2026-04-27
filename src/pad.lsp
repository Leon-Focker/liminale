;; * pads

(in-package :liminale)

(define-note-type :pad
  :min-freq 55
  :max-freq 1234
  :min-duration 10000
  :max-duration 65000
  :ratios (append
	   ;; start with ratios > 1
	   '(2 3 4 5 6 7 8 9 10)
	   '(3/2 4/3 5/4 6/5); 7/6 8/7)
	   '(5/3 6/4) ; maybe 9/7?
	   '(5/2); 7/4 8/5)
	   ;'(7/3 9/5)
	   '(7/2 8/3 9/4)
	   ;; now < 1
	   '(1/2 1/3 1/4 1/5 1/6 1/7 1/8); 1/9 1/10)
	   '(2/3 3/4 4/5); 5/6)
	   '(3/5)
	   '(2/5 5/8)
	   ;'(3/7 5/9)
	   ;'(2/7 3/8 4/9)
	   )
  :vel-mod (get-sine-modulator 300 (* pi 1/2))
  :density-mod (get-cut-off-sine-modulator 500 0.2 0.5 -0.2)
  )

;; *** generate-new-notes
;;; Given a list of currently playing pad notes, which notes should additionally
;;; start to play? Return additional notes as a list.
(defmethod generate-new-notes ((type (eql :pad)) time
			       &key active-notes
			       &allow-other-keys)
  (let* ((active-pad-notes (remove-if-not #'is-pad active-notes))
	 (density-mod-val (get-mod-value (density-mod :pad) time t))
	 (density-mult-from-mod (1+ (* -1 5 density-mod-val)))
	 (chance-per-minute-1 (* 3 density-mult-from-mod))
	 (chance-per-minute-2 (* 0.6 density-mult-from-mod))
	 (checks-per-minute (/ 1 (/ *liminale-grid-mseconds* 1000 60)))
	 (chance-per-check-1 (/ chance-per-minute-1 checks-per-minute))
	 (chance-per-check-2 (/ chance-per-minute-2 checks-per-minute))
	 (new-note
	   (make-note :start time
		      :type :pad
		      :duration (get-new-duration :pad)
		      :velocity (get-mod-value (vel-mod :pad) time)
		      :freq (apply #'get-new-frequency :pad
				   (mapcar #'note-freq active-pad-notes)))))
    (case (length active-pad-notes)
      ((0 1 2)
       (cons new-note
	     (generate-new-notes
	      :pad
	      time
	      :active-notes (cons new-note active-pad-notes))))
      ((3 4 5)
       (when (<= (random-liminale) chance-per-check-1)
	 (list new-note)))
      ((6 7)
       (when (and (< (get-mod-value (vel-mod :pad) time) 0.2)
		  (<= (random-liminale) chance-per-check-2))
	 (list new-note))))))


;; *** get-new-frequency
(defun pad-priority-comp (x y &optional (optimal-freq 285))
  (< (abs (- optimal-freq x)) (abs (- optimal-freq y))))

(defmethod get-new-frequency ((type (eql :pad)) &rest freqs)
  ;; Sorting them by #'< means that lower freqs are more important in
  ;; #'filter-similar-options
  (setf freqs (sort freqs #'<))
  (get-new-frequency-aux
   type
   freqs
   #'(lambda (ls) (prefer-first-options (sort ls #'pad-priority-comp)))))


;; EOF pad.lsp
