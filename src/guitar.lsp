;; * guita

(in-package :liminale)

;; ** types

;;; can play less harmonic pitches as transitional notes.

(define-note-type :guitar
  :min-freq 110
  :max-freq 3000
  :min-no-repetitions 5
  :remember-n-last-notes 2
  :init-dur 500
  :freq-ratios (append
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
  :last-was-pause nil
  ;; this does heavy lifting for this type:
  :freq-target nil)

;; *** get-new-duration
(defmethod get-new-duration ((type (eql :guitar)) &rest durs)
  (if (freq-target :guitar)
      ;; something short, maybe even same as last?
      (setf (min-duration :guitar) 350
	    (max-duration :guitar) 1500)
      (let ((chance-for-rest 0.2))
	(if (< (random-liminale) chance-for-rest)
	    ;; rest
	    (setf (min-duration :guitar) 15000
		  (max-duration :guitar) 84000)
	    ;; short
	    (setf (min-duration :guitar) 500
		  (max-duration :guitar) 5000))))
  (get-new-duration-aux type durs))

;; *** get-new-frequency
(defmethod get-step-towards-target ((type (eql :guitar)) how-many-steps)
  (unless (> how-many-steps 0)
    (error "get-step-towards-target:Can't get to target in ~a steps!"
	   how-many-steps))
  (let ((last (cadr (get-last-freqs :guitar)))
	(target (freq-target :guitar)))
    (round (* (expt (/ target last) (/ 1 how-many-steps)) last))))
    
;; TODO
;; maybe better way to get how-many-steps than random? for example with
;; distance between target and last...
(defmethod get-new-frequency ((type (eql :guitar)) &rest freqs)
  ;; Sorting them by #'> means that higher freqs are more important in
  ;; #'filter-similar-options
  (setf freqs (sort freqs #'>))
  (if (freq-target type)
      ;; TODO
      ;; check whether target would still be harmonic (how?), else pick new...
      ;; try to reach freq-target either in one nr n steps. 
      (let ((steps (1+ (round (* (random-liminale) 2)))))
	(prog1 (get-step-towards-target type steps)
	  (when (= 1 steps) (setf (freq-target type) nil))))
      ;; decide whether to set new freq-target
      (let ((chance-for-new-target 0.25))
	(if (< (random-liminale) chance-for-new-target)
	    ;; set new target
	    (let ((target (get-new-frequency-aux type freqs)))
	      (get-last-freqs type)
	      (setf (freq-target type) target)
	      (get-step-towards-target type (+ 2 (round (* (random-liminale) 2)))))
	    (get-new-frequency-aux type freqs)))))


;; *** get-new-note
(defmethod get-new-note ((type (eql :guitar)) time
			 &key freqs &allow-other-keys)
  (make-note :start time
	     :type type
	     :freq (apply #'get-new-frequency type freqs)
	     :duration (get-new-duration type) ; needs to be after get-new-freq
	     ;; TODO 
	     ;; :velocity (* (get-mod-value (density-mod :pad) time t) 0.75)
	     ))

;; *** generate-new-notes
;;; Given a list of currently playing notes, which notes should additionally
;;; start to play? Return additional notes as a list.
(defun is-older-pad (note)
  (and (is-pad note)
       (<= 6 (- (note-duration note) (note-time-left note)))))

(defmethod generate-new-notes ((type (eql :guitar))
			       time
			       &rest keys
			       &key active-notes
			       &allow-other-keys)
  (let ((guitar-notes (remove-if-not #'is-guitar active-notes))
	(pad-notes (remove-if-not #'is-older-pad active-notes)))
    (unless guitar-notes
      (list (apply #'get-new-note type time
		   :freqs (mapcar #'note-freq pad-notes)
		   keys)))))

;; EOF guitar.lsp
