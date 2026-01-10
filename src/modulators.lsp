;; * modulators

(in-package :relax)

(defstruct (modulator)
  (fn #'(lambda (time) (abs (sin time)))))	         	; fn(x) -> y

;; *** get-modulator-value
;;; Time in miliseconds -> some y-value
(defun get-mod-value (modulator time)
  (funcall (modulator-fn modulator) (/ time 1000)))

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
(defun get-sine-modulator (period-in-seconds)
  (make-modulator :fn #'(lambda (time-in-seconds)
			  (/ (1+ (sin (/ (* 2 pi time-in-seconds)
					 period-in-seconds)))
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

;; EOF modulators.lsp
