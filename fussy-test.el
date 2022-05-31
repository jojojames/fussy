(require 'ert)
(require 'fussy)

;; For `help--symbol-completion-table'.
(require 'help-fns)

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

(ert-deftest fussy-all-completions-fussy-filter-fn-flex-c< ()
  "Assert `fussy-filter-flex-c' is the fastest filter method."
  (dolist (query '("a" "b" "c"))
    (let* ((table 'help--symbol-completion-table)
           (pred nil)
           (point 1)
           (fussy-filter-fn 'fussy-filter-flex-c)
           (flex-c-res
            (car
             (benchmark-run 3
               (fussy-all-completions query table pred point)))))
      (should
       (< flex-c-res
          (let ((fussy-filter-fn 'fussy-filter-flex))
            (car (benchmark-run 3
                   (fussy-all-completions query table pred point))))))
      (should
       (< flex-c-res
          (let ((fussy-filter-fn 'fussy-filter-orderless))
            (car (benchmark-run 3
                   (fussy-all-completions query table pred point)))))))))

(ert-deftest fussy-filter-fn-flex-c< ()
  "Assert `fussy-filter-flex-c' is the fastest filter method."
  (dolist (query '("a" "b" "c" "def"))
    (let* ((table 'help--symbol-completion-table)
           (pred nil)
           (point 1)
           (flex-c-res
            (car (benchmark-run 3
                   (fussy-filter-flex-c query table pred point)))))
      (should
       (<
        flex-c-res
        (car (benchmark-run 3
               (fussy-filter-orderless query table pred point)))))
      (should
       (<
        flex-c-res
        (car (benchmark-run 3
               (fussy-filter-flex query table pred point))))))))

(ert-deftest fussy-filter-fn-flex-c-candidates ()
  "Assert result of `fussy-filter-flex-c' matches other filters."
  (dolist (query '("a" "b" "c" "def"))
    (let* ((table 'help--symbol-completion-table)
           (pred nil)
           (point 1)
           (flex-c-res (fussy-filter-flex-c query table pred point)))
      (should
       (= (length flex-c-res)
          (length (fussy-filter-flex query table pred point))))
      (should
       (= (length flex-c-res)
          (length (fussy-filter-orderless query table pred point)))))))

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

(ert-deftest fussy--string-without-unencodeable-chars-test ()
  "Test that unencodeable chars are removed."
  (should
   (string=
    (fussy--string-without-unencodeable-chars
     (string-as-multibyte  ";; Copyright 2022 Jo Beøˆ€’"))
    ";; Copyright 2022 Jo Be"))
  (should
   (string=
    (fussy--string-without-unencodeable-chars
     (string-as-multibyte
      ";; This buffer is for text that is not saved, and for Lisp evaluation.øˆ€€"))
    ";; This buffer is for text that is not saved, and for Lisp evaluation.")))

(ert-deftest fussy--string-without-unencodeable-chars-consult--tofu-char-test ()
  "Test that `consult--tofu-char' is removed."
  (let ((tofu (char-to-string #x200000)))
    (should
     (string=
      (fussy--string-without-unencodeable-chars (concat "jjbb" tofu))
      "jjbb"))))

(ert-deftest fussy--should-propertize-p ()
  "Test `fussy--should-propertize-p' return correct values."
  ;; use-pcm-highlight is t.
  (let ((use-pcm-highlight t)
        (fussy-filter-fn 'not-orderless)
        (fussy-propertize-fn 'something))
    (should
     (eq (fussy--should-propertize-p use-pcm-highlight) nil)))

  ;; `fussy-fitler-fn' is `orderless'.
  (let ((use-pcm-highlight nil)
        (fussy-filter-fn 'fussy-filter-orderless)
        (fussy-propertize-fn 'something))
    (should
     (eq (fussy--should-propertize-p use-pcm-highlight) nil)))

  ;; `fussy-propertize-fn' is nil.
  (let ((use-pcm-highlight nil)
        (fussy-filter-fn 'not-orderless)
        (fussy-propertize-fn nil))
    (should
     (eq (fussy--should-propertize-p use-pcm-highlight) nil)))

  ;; Should return something.
  (let ((use-pcm-highlight nil)
        (fussy-filter-fn 'not-orderless)
        (fussy-propertize-fn 'something))
    (should
     (fussy--should-propertize-p use-pcm-highlight))))

(ert-deftest fussy--using-pcm-highlight-p ()
  "Test `fussy--using-pcm-highlight-p' return correct values."
  ;; table is `completion-file-name-table'.
  (let ((table 'completion-file-name-table)
        (fussy-filter-fn 'not-orderless))
    (should
     (fussy--using-pcm-highlight-p table)))

  ;; `fussy-score-fn' returns no indices.
  (let ((table 'random-table)
        (fussy-score-fn 'fn-without-indices)
        (fussy-score-fns-without-indices '(fn-without-indices))
        (fussy-filter-fn 'not-orderless))
    (should
     (fussy--using-pcm-highlight-p table)))

  ;; `fussy-filter-fn' is using `orderless'.
  (let ((table 'completion-file-name-table)
        (fussy-score-fn 'fn-without-indices)
        (fussy-score-fns-without-indices '(fn-without-indices))
        (fussy-filter-fn 'fussy-filter-orderless))
    (should
     (eq (fussy--using-pcm-highlight-p table) nil))))
