(require 'ert)
(require 'flx-completion)

(ert-deftest flx-completion--strlen<-test ()
  (should (equal nil (flx-completion--strlen< "abc" "a")))
  (should (equal t (flx-completion--strlen< "a" "bc")))
  (should (equal nil (flx-completion--strlen< "a" "a"))))

(ert-deftest flx-completion--strlen>-test ()
  (should (equal t (flx-completion--strlen> "abc" "a")))
  (should (equal nil (flx-completion--strlen> "a" "bc")))
  (should (equal nil (flx-completion--strlen> "a" "a"))))
