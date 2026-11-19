;; * guitar

(in-package :liminale)

;; ** types

;;; can play less harmonic pitches as transitional notes.

;;; - implement melodic rules, like step after jump?
;;; - 

(define-note-type :guitar
  :min-freq 110
  :max-freq 1200
  :min-duration nil
  :max-duration nil
  :min-no-repetitions 5
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
  :freq-target nil
  :nr-steps 0)

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
(defmethod get-step-towards-target ((type (eql :guitar)) how-many-steps
				    &optional (skip-n-steps 0))
  (unless (> how-many-steps 0)
    (error "get-step-towards-target: Can't get to target in ~a steps!"
	   how-many-steps))
  (unless (and (< skip-n-steps how-many-steps) (>= skip-n-steps 0))
    (error "get-step-towards-target: Can't skip ~a of ~a steps!"
	   skip-n-steps how-many-steps))
  (let ((last (cadr (get-last-freqs :guitar)))
	(target (freq-target :guitar)))
    (let ((result (round (* (expt (/ target last)
				  (/ (1+ skip-n-steps) how-many-steps))
			    last))))
      (add-last-freq type result)
      result)))
    
;; TODO
;; maybe better way to get how-many-steps than random? for example with
;; distance between target and last...
(defmethod get-new-frequency ((type (eql :guitar)) &rest freqs)
  ;; Sorting them by #'< means that lower freqs are more important in
  ;; #'filter-similar-options
  (setf freqs (sort freqs #'<))
  (if (freq-target type)
      ;; TODO
      ;; check whether target would still be harmonic (how?), else pick new...
      ;; try to reach freq-target either in one nr n steps.
      (prog1 (get-step-towards-target type (nr-steps type))
	(decf (nr-steps type))
	(when (< (nr-steps type) 1) (setf (freq-target type) nil)))
      ;; decide whether to set new freq-target
      (let ((chance-for-new-target 0.25))
	(if (and (< (car (get-last-durs :guitar)) 8000)
		 (< (random-liminale) chance-for-new-target))
	    ;; set new target
	    (let* ((target (get-new-frequency-aux type freqs))
		   (steps (+ 2 (round (* (random-liminale) 3))))
		   (skip (min (- steps 2) (round (* (random-liminale) 2)))))
	      (setf (nr-steps type) (- steps 1 skip))
	      (get-last-freqs type)
	      (setf (freq-target type) target)
	      (get-step-towards-target type steps skip))
	    (get-new-frequency-aux type freqs)))))


;; *** get-new-note
(defmethod get-new-note ((type (eql :guitar)) time
			 &key freqs &allow-other-keys)
  (make-note :start time
	     :type type
	     :freq (apply #'get-new-frequency type freqs)
	     :duration (get-new-duration type) ; needs to be after get-new-freq
	     ;; TODO 
	     :velocity 1))

;; *** generate-new-notes
;;; Given a list of currently playing notes, which notes should additionally
;;; start to play? Return additional notes as a list.
(defmethod generate-new-notes ((type (eql :guitar))
			       time
			       &rest keys
			       &key active-notes
			       &allow-other-keys)
  (let ((guitar-notes (remove-if-not #'is-guitar active-notes))
	(pad-notes (remove-if-not #'is-older-pad active-notes)))
    (unless guitar-notes
      (let ((new-note (apply #'get-new-note type time
			     :freqs (mapcar #'note-freq pad-notes)
			     keys)))
	;; (liminale-dump-log (note-debug new-note))
	(list new-note)))))

;; EOF guitar.lsp
