;; * contemplative

(in-package :liminale)

;; ** types

(define-note-type :contemplative
  :min-freq 110
  :max-freq 3000
  :min-duration 400
  :max-duration 2000
  :min-no-repetitions 0
  :remember-n-last-notes 20
  :init-dur 500
  :freq-ratios '(1 2 3 4 6 8)
  )

(define-note-type :contemplative-pause
  :min-duration 5000
  :max-duration 25000
  )

;; *** get-new-frequency
(defmethod get-new-frequency ((type (eql :contemplative)) &rest freqs)
  ;; Sorting them by #'> means that higher freqs are more important in
  ;; #'filter-similar-options
  (setf freqs (sort freqs #'>))
  ;; include some remembered freqs at the end, because they might still echo
  ;; (append freqs (first-n (get-last-freqs type) 2))
  ;; call to aux method
  (get-new-frequency-aux
   type
   freqs
   #'(lambda (ls) (or (find (car (get-last-freqs type))
		       ls
		       :test #'(lambda (x y) (<= x y)))
		      (first ls)))))

;; *** get-new-note
(defmethod get-new-note ((type (eql :contemplative)) time
			   &key freqs &allow-other-keys)
    (make-note :start time
	       :duration (get-new-duration type)
	       :type type
	       ;; between 0.0 and 0.75
	       :velocity (* (get-mod-value (density-mod :pad) time t) 0.75)
	       :freq (apply #'get-new-frequency type freqs)))



(defmethod get-new-note ((type (eql :contemplative-pause)) time
			   &key &allow-other-keys)
    (make-note :start time
	       :type type
	       :duration (get-new-duration type)
	       :velocity 0.0
	       :freq 0))

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
				 &rest keys
				 &key active-notes
				 &allow-other-keys)
    (when (<= time 0) (setf last-was-pause nil nr-of-reps 0))
    (let ((contemplative-notes (remove-if-not #'is-contemplative active-notes))
	  (pause-notes (remove-if-not #'is-contemplative-pause active-notes))
	  (pad-notes (remove-if-not #'is-older-pad active-notes)))
      (unless (append contemplative-notes pause-notes)
	(if last-was-pause
	    ;; generate :contemplative notes
	    (prog1
		(list
		 (apply #'get-new-note type time
			:freqs (mapcar #'note-freq
				       (append contemplative-notes
					       pad-notes))
			keys))
	      (incf nr-of-reps)
	      ;; determines number of short notes after another
	      (when (< (random-liminale) (* nr-of-reps 0.08))
		(setf last-was-pause nil
		      nr-of-reps 0)))
	    ;; generate :contemplative-pause notes
	    (apply #'generate-new-notes :contemplative-pause time keys)))))
  
  (defmethod generate-new-notes ((type (eql :contemplative-pause)) time
				 &rest keys
				 &key &allow-other-keys)
    (setf last-was-pause t)
    (list (apply #'get-new-note :contemplative-pause time keys)))

  ;; *** get-new-duration
  (defmethod get-new-duration ((type (eql :contemplative)) &rest durs)
    (if last-was-pause
	(get-new-duration-aux type durs)
	(let ((last-dur (car (get-last-durs type))))
	  (progn (add-last-dur type last-dur) last-dur)))))

;; *** reset
;;; becasue :contemplative also calls :contemplative-pause, we need this hook
;;; to reset the latter:
(defmethod reset-note-type :after ((type (eql :contemplative))
				   &key &allow-other-keys)
  (reset-note-type :contemplative-pause))

;; EOF contemplative.lsp
