(require 'ert)
(require 'fussy)

(defvar fussy-history-variable '())

(ert-deftest fussy--strlen<-test ()
  (should (equal nil (fussy-strlen< "abc" "a")))
  (should (equal t (fussy-strlen< "a" "bc")))
  (should (equal nil (fussy-strlen< "a" "a"))))

(ert-deftest fussy--strlen>-test ()
  (should (equal t (fussy-strlen> "abc" "a")))
  (should (equal nil (fussy-strlen> "a" "bc")))
  (should (equal nil (fussy-strlen> "a" "a"))))

(ert-deftest fussy-histlen<-test--benchmark ()
  (setq fussy-history-variable '("first"
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
                    (fussy-histlen< "xyz" "abc"))))))

(ert-deftest fussy-histlen<-test ()
  (setq fussy-history-variable '("first" "second"))
  (let ((minibuffer-history-variable 'fussy-history-variable))
    (should (equal t (fussy-histlen< "first" "second")))
    (should (equal nil (fussy-histlen< "second" "first")))
    (should (equal nil (fussy-histlen< "doesntexist" "first")))
    (should (equal t (fussy-histlen< "second" "doesntexist")))
    (should (equal nil (fussy-histlen< "doesntexist" "doesntexist")))))

(ert-deftest fussy-histlen->strlen<--benchmark ()
  (setq fussy-history-variable '("first"
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
                    (fussy-histlen->strlen< "twelve" "eleven"))))))

(ert-deftest fussy-histlen->strlen< ()
  (setq fussy-history-variable '("first" "second"))
  (let ((minibuffer-history-variable 'fussy-history-variable))
    (should (equal t (fussy-histlen->strlen< "first" "second")))
    (should (equal nil (fussy-histlen->strlen< "second" "first")))
    (should (equal nil (fussy-histlen->strlen< "doesntexist" "first")))
    (should (equal t (fussy-histlen->strlen< "second" "doesntexist")))
    (should
     (equal nil (fussy-histlen->strlen< "doesntexist" "doesntexist")))
    (should (equal nil (fussy-histlen->strlen< "longerstring" "short")))
    (should (equal t (fussy-histlen->strlen< "short" "longerstring")))))

(ert-deftest fussy--score--cache ()
  (let*
      ((fussy-score-fn 'flx-score)
       (candidates
        '("~/.emacs.d/straight/repos/orderless/orderless.el"
          "~/Code/yyoshereios/iOSTest/yyosHereiPadRootViewController.h"))
       (string-cache-res
        (fussy--score candidates "odor" nil
                      flx-strings-cache))
       (file-cache-res
        (fussy--score candidates "odor" nil
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
