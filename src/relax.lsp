(in-package :relax)

;; * relaxing...

(format t "~& ~&relaxing now :3~& ~&")

(defparameter *acc-samples*
  (soundpile-from-folder 'accordion "/E/relax/acc_samples/" :analyse t))

(wsound "this is a test" 2
  (fplay 0 100
    (soundpile *acc-samples*)
    (sound (nth-mod (random 100) (data soundpile)))
    (rhythm (random 5))
    (duration (+ 5 (random 30)))
    (amp-env '(0 1  90 1  100 0))
    (srt (+ 0.1 (random 0.1)))
    (amp (+ 0.5 (random 0.2)))
    (degree (random 90))))

(stop-playing)

(wsound "sine-test" 1
  (clm::simple-sine 0 1 214 0.5))

;; EOF relax.lsp
