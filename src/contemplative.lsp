;; * contemplative

(in-package :liminale)

;; ** types

(define-note-type :contemplative
  :min-freq 110
  :max-freq 3000
  :min-duration 500
  :max-duration 2000
  :min-no-repetitions 4 ; 0
  :init-dur 500
  :freq-ratios '(1/2 1 3/2 2 3 4 6 8)
  :nr-of-reps 0
  :last-was-pause nil
  :nth-harmonic 1)

;; *** get-new-duration
(defmethod get-new-duration ((type (eql :contemplative)) &rest durs)
  (if (and (last-was-pause type) (> (nr-of-reps type) 0))
      (let ((last-dur (car (get-last-durs type))))
	(add-last-dur type last-dur)
	last-dur)
      (progn
	(if (last-was-pause type)
	    (setf (min-duration type) 500
		  (max-duration type) 2000)
	    (setf (min-duration type) 5000
		  (max-duration type) 25000))
	(get-new-duration-aux type durs))))

;; *** get-new-frequency
;; (let ((fundamental (nth-mod (1- (nth-harmonic type))
;; 			    (get-last-freqs type))))
;;   (if (find fundamental ls :test #'similar-freqp)
;;       (progn (incf (nth-harmonic type))
;; 	     (scale-until-in-range
;; 	      (* (nth-harmonic type) fundamental)
;; 	      (min-freq type)
;; 	      (max-freq type)))
;;       (progn (setf (nth-harmonic type) 1)
;; 	     nil)))
(defmethod get-new-frequency ((type (eql :contemplative)) &rest freqs)
  (flet ((picking-fn (ls)
	   ;; add pitches
	   (let ((fundamental (nth-mod (1- (nth-harmonic type))
				       (get-last-freqs type))))
	     (incf (nth-harmonic type))
	     (when t ;;(find fundamental ls :test #'similar-freqp)
	       (loop for k from (nth-harmonic type) to 9
		     do (push (* fundamental k) ls))))
	   ;; sort
	   (setf ls (sort (pick-original-options ls (get-last-freqs type) #'similar-freqp)
			  #'<))
	   ;; choose
	   (or (find (car (get-last-freqs type)) ls
		     :test #'(lambda (x y) (< x y)))
	       (first ls))))
    ;; Sorting them by #'> means that higher freqs are more important in
    ;; #'filter-similar-options
    (setf freqs (sort freqs #'>))
    ;; include some remembered freqs at the end, because they might still echo
    ;; (append freqs (first-n (get-last-freqs type) 2))
    ;; call to aux method
    (scale-until-in-range
     (get-new-frequency-aux type freqs #'picking-fn)
     (min-freq type)
     (max-freq type))))

;; *** get-new-note
(defmethod get-new-note ((type (eql :contemplative)) time
			 &key freqs &allow-other-keys)
  (let ((vel (if (last-was-pause type)
		 (* (get-mod-value (density-mod :pad) time t) 0.75)
		 0))
	(freq (if (last-was-pause type)
		  (apply #'get-new-frequency type freqs)
		  0)))
    (make-note :start time
	       :duration (get-new-duration type)
	       :type type
	       ;; between 0.0 and 0.75
	       :velocity vel
	       :freq freq)))

;; *** generate-new-notes
;;; Given a list of currently playing notes, which notes should additionally
;;; start to play? Return additional notes as a list.
(defun is-older-pad (note)
  (and (is-pad note)
       (<= 6 (- (note-duration note) (note-time-left note)))))

(defmethod generate-new-notes ((type (eql :contemplative))
			       time
			       &rest keys
			       &key active-notes
			       &allow-other-keys)
  (let ((contemplative-notes (remove-if-not #'is-contemplative active-notes))
	(pad-notes (remove-if-not #'is-older-pad active-notes)))
    ;; get new note when none is playing
    (unless contemplative-notes
      (let ((end-run-threshold
	      (* (- 1.3 (get-mod-value (density-mod :pad) time t)) 0.1))    
	    (new-note (apply #'get-new-note type time
			     :freqs (mapcar #'note-freq pad-notes)	
			     keys)))
	;; decide whether next note is a pause or not 
	(if (last-was-pause type)
	    (if (< (random-liminale) (* (nr-of-reps type) end-run-threshold))
		(progn (setf (last-was-pause type) nil
			     (nr-of-reps type) 0))
		(incf (nr-of-reps type)))
	    (progn (setf (last-was-pause type) t)
		   (setf (nth-harmonic type) 1)))
	;; return
	(list new-note)))))

;; EOF contemplative.lsp
