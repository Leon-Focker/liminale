;; * helpers.lsp

(in-package :relax)

;; ** CLM

(defmacro wsound (name out-channels &body body)
  `(with-sound (:header-type clm::mus-riff :sampling-rate 48000
		:output (format nil "~a~a" (relax-path ,name) ".wav")
		:channels ,out-channels :play t :scaled-to 0.98
		:force-recomputation nil)
     ,@body))

;; EOF helpers.lsp
