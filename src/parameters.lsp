;; * parameters

;; ** Duration

(defparameter *liminale-grid-mseconds* 100)
(defparameter *min-no-repetitions* 5)
(defparameter *min-duration-con* 400)
(defparameter *max-duration-con* 2000)
(defparameter *min-duration-con-pause* 5000)
(defparameter *max-duration-con-pause* 25000)
(defparameter *min-duration-pad* 10000)
(defparameter *max-duration-pad* 65000)
(defparameter *min-duration-noise* 120000)
(defparameter *max-duration-noise* 300000)


;; ** Frequency

(defparameter *min-freq* 55)
(defparameter *max-freq* 1234)

(defparameter *pad-ratios*
  (append
   ;; start with ratios > 1
   '(2 3 4 5 6 7 8 9 10)
   '(3/2 4/3 5/4 6/5 7/6 8/7)
   '(5/3 6/4) ; maybe 9/7?
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

(defparameter *con-ratios*
  (append
   ;; start with ratios > 1
   '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
   '(3/2 4/3 5/4 6/5 7/6 8/7)
   '(5/3 6/4) ; maybe 9/7?
   '(5/2 7/4 8/5)
   '(7/3 9/5)
   '(7/2 8/3 9/4)
   ;; now < 1
   ;; '(1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9 1/10)
   ;; '(2/3 3/4 4/5 5/6)
   ;; '(3/5)
   ;; '(2/5 5/8)
   ;; '(3/7 5/9)
   ;; '(2/7 3/8 4/9)
   ))


;; ** Modulation

(defparameter *pad-velocity-period* 300) ; in seconds
(defparameter *contemplative-velocity-period* 500) ; in seconds
;; global so Pads can also access it
(defparameter *contemplative-vel-mod*
  (get-cut-off-sine-modulator *contemplative-velocity-period* 0.2 0.5 -0.2))


;; EOF parameters.lsp
