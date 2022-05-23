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
