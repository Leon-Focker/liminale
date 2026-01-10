;; * modulators

(in-package :relax)

(defstruct (modulator)
  (fn #'(lambda (time) (abs (sin time)))))	         	; fn(x) -> y

;; *** get-modulator-value
;;; Time in miliseconds -> some y-value
(defun get-mod-value (modulator time-in-ms)
  (funcall (modulator-fn modulator) (/ time-in-ms 1000)))

;; ** some modulators

;;; *** get-sine-modulator
;;; A simple sine-wave between 0 and 1 with period in seconds.
;;; Example:
#|
(visualize (loop for i from 0 to 100
		 collect (get-mod-value (get-sine-modulator 100) (* 1000 i))))

"           ___________                                          " 
"       ____           ____                                      " 
"     __                   __                                    " 
"  ___                       ____                                " 
"__                              _                              _" 
"                                 ____                       ___ " 
"                                     __                   __    " 
"                                       ____           ____      " 
"                                           ___________          " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                "
|#
(defun get-sine-modulator (period-in-seconds &optional (phase-offset 0))
  (make-modulator :fn #'(lambda (time-in-seconds)
			  (/ (1+ (sin (+ (/ (* 2 pi time-in-seconds)
					    period-in-seconds)
					 phase-offset)))
			     2))))

;;; *** get-saw-modulator
;;; A simple saw-wave between 0 and 1 with period in seconds.
;;; Example:
#|
(visualize (loop for i from 0 to 100
		 collect (get-mod-value (get-saw-modulator 50) (* 1000 i))))
"                              __                             ___" 
"                          ____                            ___   " 
"                      ____                            ____      " 
"                  ____                            ____          " 
"              ____                            ____              " 
"           ___                            ____                  " 
"       ____                            ___                      " 
"   ____                            ____                         " 
"___                             ___                             " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                "
|#
(defun get-saw-modulator (period-in-seconds)
  (make-modulator :fn #'(lambda (time-in-seconds)
			  (/ (mod time-in-seconds period-in-seconds)
			     period-in-seconds))))

;; *** get-cut-off-sine-modulator
;;; Example:
#|
(visualize (loop for i from 0 to 100
		 collect (get-mod-value (get-cut-off-sine-modulator 33)
					(* 1000 i))))

"          ___                  __                   __          " 
"         _   _                _  __                _  __        " 
"        _                    _                    _             " 
"       _      _             _      _             _      _       " 
"               _                    _                    _      " 
"      _                    _                    _               " 
"                _         _          _         _          _     " 
"     _           _                    _                         " 
"_____             ________             ________            _____" 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
"                                                                " 
|#
(defun get-cut-off-sine-modulator (period-in-seconds
				   &optional (from-bottom 0.3) (from-top 0)
				     (y-offset -0.3) (phase-offset (* pi 3/2)))
  (make-modulator
   :fn #'(lambda (time-in-seconds)
	   (+ (max (min (/ (1+ (sin (+ (/ (* 2 pi time-in-seconds)
					  period-in-seconds)
				       phase-offset)))
			   2)
			(- 1 from-top))
		   from-bottom)
	      y-offset))))

;; EOF modulators.lsp
