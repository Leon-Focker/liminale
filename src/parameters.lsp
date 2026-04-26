;; * parameters

(in-package :liminale)

(setf *verbose* nil)
(setf *liminale-grid-mseconds* 100)

;; ** Modulation

(defparameter *pad-velocity-period* 300) ; in seconds
(defparameter *contemplative-velocity-period* 500) ; in seconds
;; global so Pads can also access it
(defparameter *contemplative-vel-mod*
  (get-cut-off-sine-modulator *contemplative-velocity-period* 0.2 0.5 -0.2))


;; EOF parameters.lsp
