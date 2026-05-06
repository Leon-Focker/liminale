;; * piano

(in-package :liminale)

(setf *liminale-grid-mseconds* 70)

(define-note-type :piano-chords
  :min-freq 27
  :max-freq 2000
  :min-duration (* 8 4 *liminale-grid-mseconds*)
  :max-duration (* 64 4 *liminale-grid-mseconds*)
  :min-no-repetitions 1
  :freq-ratios nil
  :dur-ratios nil)

(setf (freq-ratios :piano-chords)
      (append
       ;; start with ratios > 1
       '(2 3 4 5 6 7 8 9 10)
       '(3/2 4/3 5/4 6/5 7/6 8/7)
       '(5/3 6/4 9/7)
       '(5/2 7/4 8/5)
       '(7/3 9/5)
       '(7/2 8/3 9/4)
       ;; now < 1
       '(1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9 1/10)
       '(2/3 3/4 4/5 5/6)
       '(3/5)
       '(2/5 5/8)
       '(3/7 5/9)
       '(2/7 3/8 4/9)))

;; (setf (freq-ratios :piano-chords)
;;       (mapcar #'equal-tempered-multiplier
;; 	      '(1 3 4 5 7 8 9 12 15 16 17 19 24 29 31 36
;; 	        -2 -5 -7 -8 -9 -12 -17 -19 -24 -36)))

(setf (dur-ratios :piano-chords)
      (append
       '(2 3 4 6 8)
       '(2/3 3/4)
       '(1/2 1/3 1/4 1/6 1/8)
       '(3/2 4/3)))

;; (append
;;  '(3 5 6 7 9 10)
;;  '(3/2 4/3 5/4 6/5 7/6 8/7 9/8)
;;  '(5/2 7/4)
;;  '(1/3 1/5 1/6 1/10)
;;  '(2/3 3/4 4/5 5/6 6/7 7/8 8/9)
;;  '(2/5 4/7))

;; *** generate-new-notes
(defmethod get-new-note ((type (eql :piano-chords)) time
			       &key durs freqs &allow-other-keys)
  (make-note :start time
	     :type type
	     :duration (apply #'get-new-duration type durs)
	     :velocity 0.7
	     :freq (apply #'get-new-frequency type freqs)))

(defmethod generate-new-notes ((type (eql :piano-chords)) time
			       &key active-notes
			       &allow-other-keys)
  (case (length active-notes)
    ((0 1 2 3 4 5 6)
     (let* ((durs (mapcar #'note-duration active-notes))
	    (freqs (mapcar #'note-freq active-notes))
 	    (new-note (get-new-note type time :durs durs :freqs freqs)))
       (cons new-note
	     (generate-new-notes :piano-chords time
				 :active-notes (cons new-note active-notes)))))))

;; *** get-new-duration
(defmethod get-new-duration ((type (eql :piano-chords)) &rest durs)
  (get-new-duration-aux
   type durs #'(lambda (ls) (prefer-first-options (sort ls #'>)))))

;; *** get-new-frequency
(defun chord-priority-comp (x y &optional (optimal-freq 285))
  (< (abs (- optimal-freq x)) (abs (- optimal-freq y))))

(defmethod get-new-frequency ((type (eql :piano-chords)) &rest freqs)
  ;; Sorting them by #'< means that lower freqs are more important in
  ;; #'filter-similar-options
  (setf freqs (sort freqs #'<))
  (get-new-frequency-aux
   type
   freqs
   #'(lambda (ls) (nth (floor (* (random-liminale) (length ls))) ls))))
  ; #'(lambda (ls) (prefer-first-options (sort ls #'chord-priority-comp)))))

;; Generate the Midi File:
#+nil(notes-to-midi (generate-notes 100 '(:piano-chords))
		    "/home/leon/piano-chords.mid")

;; EOF piano.lsp
