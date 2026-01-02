;; * helpers.lsp

(in-package :relax)

;; ** randomness

(let* ((seed 5)
       (rn (make-random-number seed)))
  
  (defun random-relax ()
    (get-next rn))
  
  (defun reset-random-relax ()
    (setf rn (make-random-number seed))
    (random-relax))
    
  (defun set-relax-seed (new-seed)
    (setf seed new-seed)
    (reset-random-relax)))

;; ** CLM

(defmacro wsound (name out-channels &body body)
  `(with-sound (:header-type clm::mus-riff :sampling-rate 48000
		:output (format nil "~a~a" (relax-path ,name) ".wav")
		:channels ,out-channels :play t :scaled-to 0.98
		:force-recomputation nil)
     ,@body))

;; EOF helpers.lsp
