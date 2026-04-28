;; * contemplative

(in-package :liminale)

;; ** types

(define-note-type :contemplative
  :min-freq 110
  :max-freq 1234
  :min-duration 400
  :max-duration 2000
  :min-no-repetitions 0
  :remember-n-last-notes 20
  :init-dur 500
  :freq-ratios (append
		;; start with ratios > 1
		'(2 3 4 5 6 7 8 9 10 11 12 13 14 15) ; 16 17 18 19 20 21)
		'(3/2 4/3 5/4 6/5 7/6 8/7)
		'(5/3 6/4)   ; maybe 9/7?
		'(5/2 7/4 8/5)
		'(7/3 9/5)
		'(7/2 8/3 9/4)
		;; now < 1
		;; '(1/2 1/3 1/4) ; 1/5 1/6 1/7 1/8 1/9 1/10)
		;; '(2/3 3/4 4/5 5/6)
		;; '(3/5)
		;; '(2/5 5/8)
		;; '(3/7 5/9)
		;; '(2/7 3/8 4/9)
		)
  )

(define-note-type :contemplative-pause
  :min-duration 5000
  :max-duration 25000
  )

;; *** generate-new-notes
;;; Given a list of currently playing notes, which notes should additionally
;;; start to play? Return additional notes as a list.
(defun is-older-pad (note)
  (and (is-pad note)
       (<= 6 (- (note-duration note) (note-time-left note)))))

(let ((last-was-pause nil)
      (nr-of-reps 0))

  (defmethod generate-new-notes ((type (eql :contemplative))
				 time
				 &key active-notes
				 &allow-other-keys)
    (when (<= time 0) (setf last-was-pause nil))
    (let ((contemplative-notes (remove-if-not #'is-contemplative active-notes))
	  (pause-notes (remove-if-not #'is-contemplative-pause active-notes))
	  (pad-notes (remove-if-not #'is-older-pad active-notes)))
      (when (null (append contemplative-notes pause-notes))
	(if last-was-pause
	    ;; generate :contemplative notes
	    (let ((duration (get-new-duration :contemplative)))
	      (incf nr-of-reps)
	      ;; determines number of short notes after another
	      (if (< (random-liminale) (* nr-of-reps 0.08))
		  (setf last-was-pause nil
			nr-of-reps 0))
	      (list
	       (make-note :start time
			  :duration duration
			  :type :contemplative
			  ;; between 0.0 and 0.75
			  :velocity (* (get-mod-value (density-mod :pad) time t) 0.75)
			  :delay-time (scale-until-in-range (/ duration 1000) 0.2 0.45 3)
			  :freq (apply #'get-new-frequency
				       :contemplative
				       (mapcar #'note-freq
					       (append contemplative-notes
						       pad-notes))))))    
	    ;; generate :contemplative-pause notes
	    (generate-new-notes :contemplative-pause time)))))
  
  (defmethod generate-new-notes ((type (eql :contemplative-pause)) time
				 &key &allow-other-keys)
    (setf last-was-pause t)
    (list
     (make-note :start time
		:duration (get-new-duration :contemplative-pause)
		:type :contemplative-pause
		:velocity 0.0
		:freq 0)))

  ;; *** get-new-duration
  (defmethod get-new-duration ((type (eql :contemplative)) &rest durs)
    (if last-was-pause
	(get-new-duration-aux :contemplative durs)
	(let ((last-dur (car (get-last-durs type))))
	  (progn (add-last-dur :contemplative last-dur) last-dur)))))

    
;; *** get-new-frequency
(defmethod get-new-frequency ((type (eql :contemplative)) &rest freqs)
  ;; Sorting them by #'> means that higher freqs are more important in
  ;; #'filter-similar-options
  (setf freqs (sort freqs #'>))
  ;; include some remembered freqs at the end, because they might still echo
  (append freqs (first-n (get-last-freqs type) 2))
  ;; call to aux method
  (get-new-frequency-aux
   type
   freqs 
   #'(lambda (ls) (or (find (car (get-last-freqs type))
		       ls
		       :test #'(lambda (x y) (<= x y)))
		 (first ls)))))

;; *** reset
;;; becasue :contemplative also calls :contemplative-pause, we need this hook
;;; to reset the latter:
(defmethod reset-note-type :after ((type (eql :contemplative))
				   &key &allow-other-keys)
  (reset-note-type :contemplative-pause))

;; EOF contemplative.lsp
