(require 'ert)
(require 'flx-completion)

(defvar flx-completion-history-variable '())

(ert-deftest flx-completion--strlen<-test ()
  (should (equal nil (flx-completion-strlen< "abc" "a")))
  (should (equal t (flx-completion-strlen< "a" "bc")))
  (should (equal nil (flx-completion-strlen< "a" "a"))))

(ert-deftest flx-completion--strlen>-test ()
  (should (equal t (flx-completion-strlen> "abc" "a")))
  (should (equal nil (flx-completion-strlen> "a" "bc")))
  (should (equal nil (flx-completion-strlen> "a" "a"))))

(ert-deftest flx-completion-histlen<-test--benchmark ()
  (setq flx-completion-history-variable '("first"
                                          "second"
                                          "three"
                                          "four"
                                          "five"
                                          "six"
                                          "seven"
                                          "eight"
                                          "nine"
                                          "ten"))
  (should (> .00009
             (car (benchmark-run 1
                    (flx-completion-histlen< "xyz" "abc"))))))

(ert-deftest flx-completion-histlen<-test ()
  (setq flx-completion-history-variable '("first" "second"))
  (let ((minibuffer-history-variable 'flx-completion-history-variable))
    (should (equal t (flx-completion-histlen< "first" "second")))
    (should (equal nil (flx-completion-histlen< "second" "first")))
    (should (equal nil (flx-completion-histlen< "doesntexist" "first")))
    (should (equal t (flx-completion-histlen< "second" "doesntexist")))
    (should (equal nil (flx-completion-histlen< "doesntexist" "doesntexist")))))

(ert-deftest flx-completion-histlen->strlen<--benchmark ()
  (setq flx-completion-history-variable '("first"
                                          "second"
                                          "three"
                                          "four"
                                          "five"
                                          "six"
                                          "seven"
                                          "eight"
                                          "nine"
                                          "ten"))
  (should (> .00009
             (car (benchmark-run 1
                    (flx-completion-histlen->strlen< "twelve" "eleven"))))))

(ert-deftest flx-completion-histlen->strlen< ()
  (setq flx-completion-history-variable '("first" "second"))
  (let ((minibuffer-history-variable 'flx-completion-history-variable))
    (should (equal t (flx-completion-histlen->strlen< "first" "second")))
    (should (equal nil (flx-completion-histlen->strlen< "second" "first")))
    (should (equal nil (flx-completion-histlen->strlen< "doesntexist" "first")))
    (should (equal t (flx-completion-histlen->strlen< "second" "doesntexist")))
    (should
     (equal nil (flx-completion-histlen->strlen< "doesntexist" "doesntexist")))
    (should (equal nil (flx-completion-histlen->strlen< "longerstring" "short")))
    (should (equal t (flx-completion-histlen->strlen< "short" "longerstring")))))

(ert-deftest flx-completion--score--cache ()
  (let*
      ((flx-completion-score-fn 'flx-score)
       (candidates
        '("~/.emacs.d/straight/repos/orderless/orderless.el"
          "~/Code/yyoshereios/iOSTest/yyosHereiPadRootViewController.h"))
       (string-cache-res
        (flx-completion--score candidates "odor" nil
                               flx-strings-cache))
       (file-cache-res
        (flx-completion--score candidates "odor" nil
                               flx-file-cache)))

    ;; With `flx-strings-cache' candidate 1 loses to candidate 2 which is
    ;; not desirable for filenames.
    (should
     (< (get-text-property 0 'completion-score (nth 0 string-cache-res))
        (get-text-property 0 'completion-score (nth 1 string-cache-res))))

    ;; With `flx-file-cache', candidate 1 wins against candidate 2 which shows
    ;; `flx-file-cache' is more appropriate than `flx-strings-cache' for
    ;; filenames.
    (should
     (> (get-text-property 0 'completion-score (nth 0 file-cache-res))
        (get-text-property 0 'completion-score (nth 1 file-cache-res))))))
