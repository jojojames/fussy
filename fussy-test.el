(require 'ert)
(require 'fussy)

;; For `help--symbol-completion-table'.
(require 'help-fns)

(defconst fussy--consult--tofu-char #x200000
  "Special character used to encode line prefixes for disambiguation.
We use invalid characters outside the Unicode range.

This is a copy of `consult--tofu-char' that we've copied over here so that we
can strip out this character for `consult' specific functions that encode
the character into the candidate.

See `fussy-without-tofu-char'.")

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

(ert-deftest fussy-all-completions-fussy-filter-fn-fast< ()
  "Assert `fussy-filter-fast' is the fastest filter method."
  (dolist (query '("a" "b" "c"))
    (let* ((table 'help--symbol-completion-table)
           (pred nil)
           (point 1)
           (fussy-filter-fn 'fussy-filter-fast)
           (fast-res
            (car
             (benchmark-run 3
               (fussy-all-completions query table pred point)))))
      (should
       (< fast-res
          (let ((fussy-filter-fn 'fussy-filter-flex))
            (car (benchmark-run 3
                   (fussy-all-completions query table pred point))))))
      (should
       (< fast-res
          (let ((fussy-filter-fn 'fussy-filter-orderless))
            (car (benchmark-run 3
                   (fussy-all-completions query table pred point)))))))))

(ert-deftest fussy-filter-fn-fast< ()
  "Assert `fussy-filter-fast' is the fastest filter method."
  (dolist (query '("a" "b" "c" "def"))
    (let* ((table 'help--symbol-completion-table)
           (pred nil)
           (point 1)
           (fast-res
            (car (benchmark-run 3
                   (fussy-filter-fast query table pred point)))))
      (should
       (<
        fast-res
        (car (benchmark-run 3
               (fussy-filter-orderless query table pred point)))))
      (should
       (<
        fast-res
        (car (benchmark-run 3
               (fussy-filter-flex query table pred point))))))))

(ert-deftest fussy-filter-fn-fast-candidates ()
  "Assert result of `fussy-filter-fast' matches other filters."
  (dolist (query '("a" "b" "c" "def"))
    (let* ((table 'help--symbol-completion-table)
           (pred nil)
           (point 1)
           (fast-res (fussy-filter-fast query table pred point)))
      (should
       (= (length fast-res)
          (length (fussy-filter-flex query table pred point))))
      (should
       (= (length fast-res)
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
        (fussy--score candidates "odor" flx-strings-cache))
       (file-cache-res
        (fussy--score candidates "odor" flx-file-cache)))

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

(ert-deftest fussy-without-unencodeable-chars-test ()
  "Test that unencodeable chars are removed."
  (should
   (string=
    (fussy-without-unencodeable-chars
     (string-as-multibyte  ";; Copyright 2022 Jo Beøˆ€’"))
    ";; Copyright 2022 Jo Be"))
  (should
   (string=
    (fussy-without-unencodeable-chars
     (string-as-multibyte
      ";; This buffer is for text that is not saved, and for Lisp evaluation.øˆ€€"))
    ";; This buffer is for text that is not saved, and for Lisp evaluation.")))

(ert-deftest fussy-without-unencodeable-chars-consult--tofu-char-test ()
  "Test that `consult--tofu-char' is removed."
  (should
   (string=
    (fussy-without-unencodeable-chars (concat "jjbb" (char-to-string #x200000)))
    "jjbb")))

(ert-deftest fussy-without-tofu-char-test ()
  "Test `fussy-without-tofu-char'."
  (should
   (string=
    (fussy-without-tofu-char
     (concat "jjbb" (char-to-string fussy--consult--tofu-char)))
    "jjbb"))
  (should
   (string=
    (fussy-without-tofu-char
     (string-as-multibyte  ";; Copyright 2022 Jo Beøˆ€’"))
    ";; Copyright 2022 Jo Be"))
  (should
   (string=
    (fussy-without-tofu-char
     (string-as-multibyte
      ";; This buffer is for text that is not saved, and for Lisp evaluation.øˆ€€"))
    ";; This buffer is for text that is not saved, and for Lisp evaluation.")))

(ert-deftest fussy-without-tofu-char-good-input-test ()
  "Test `fussy-without-tofu-char'."
  (should
   (string=
    (fussy-without-tofu-char "bb") "bb"))
  (should
   (string=
    (fussy-without-tofu-char "jj") "jj")))

(ert-deftest fussy-without-tofu-char-perf-test ()
  "Test `fussy-without-tofu-char' performance.

This test asserts `fussy-without-tofu-char' is much much faster than
`fussy-without-unencodeable-chars'."
  (let* ((tofu (char-to-string fussy--consult--tofu-char))
         (string-1 (concat "jjbb" tofu))
         (string-2 (string-as-multibyte ";; Copyright 2022 Jo Beøˆ€’"))
         (string-3 (string-as-multibyte ";; This buffer is for text that is not saved, and for Lisp evaluation.øˆ€€"))
         (performance-factor 200))
    (should
     (<
      (* performance-factor
         (car (benchmark-run 1000 (fussy-without-tofu-char string-1))))
      (car (benchmark-run 1000 (fussy-without-unencodeable-chars string-1)))))
    (should
     (<
      (* performance-factor
         (car (benchmark-run 1000 (fussy-without-tofu-char string-2))))
      (car (benchmark-run 1000 (fussy-without-unencodeable-chars string-2)))))
    (should
     (<
      (* performance-factor
         (car (benchmark-run 1000 (fussy-without-tofu-char string-3))))
      (car (benchmark-run 1000 (fussy-without-unencodeable-chars string-3)))))))

(ert-deftest fussy-encode-coding-string-test ()
  "Test `fussy-encode-coding-string'."
  (should
   (string=
    (fussy-encode-coding-string
     (concat "jjbb" (char-to-string fussy--consult--tofu-char)))
    "jjbb\370\210\200\200\200"))
  (should
   (string=
    (fussy-encode-coding-string
     (string-as-multibyte  ";; Copyright 2022 Jo Beøˆ€’"))
    ";; Copyright 2022 Jo Be\370\210\200\201\222"))
  (should
   (string=
    (fussy-encode-coding-string
     (string-as-multibyte
      ";; This buffer is for text that is not saved, and for Lisp evaluation.øˆ€€"))
    ";; This buffer is for text that is not saved, and for Lisp evaluation.\370\210\200\200\201")))

(ert-deftest fussy-encode-coding-string-good-input-test ()
  "Test `fussy-encode-coding-string'."
  (should
   (string=
    (fussy-encode-coding-string "bb") "bb"))
  (should
   (string=
    (fussy-encode-coding-string "jj") "jj")))

(ert-deftest fussy-encode-coding-string-perf-test ()
  "Test `fussy-encode-coding-string' performance.

This test asserts `fussy-encode-coding-string' is much much faster than
`fussy-without-unencodeable-chars'."
  (let* ((tofu (char-to-string fussy--consult--tofu-char))
         (string-1 (concat "jjbb" tofu))
         (string-2 (string-as-multibyte ";; Copyright 2022 Jo Beøˆ€’"))
         (string-3 (string-as-multibyte ";; This buffer is for text that is not saved, and for Lisp evaluation.øˆ€€"))
         (performance-factor 100))
    (should
     (<
      (* performance-factor
         (car (benchmark-run 1000 (fussy-encode-coding-string string-1))))
      (car (benchmark-run 1000 (fussy-without-unencodeable-chars string-1)))))
    (should
     (<
      (* performance-factor
         (car (benchmark-run 1000 (fussy-encode-coding-string string-2))))
      (car (benchmark-run 1000 (fussy-without-unencodeable-chars string-2)))))
    (should
     (<
      (* performance-factor
         (car (benchmark-run 1000 (fussy-encode-coding-string string-3))))
      (car (benchmark-run 1000 (fussy-without-unencodeable-chars string-3)))))))

(ert-deftest fussy--should-propertize-p ()
  "Test `fussy--should-propertize-p' return correct values."
  ;; use-pcm-highlight is t.
  (cl-letf* (((symbol-function 'fussy--using-pcm-highlight-p)
              (lambda () t))
             (fussy-filter-fn 'not-orderless)
             (fussy-propertize-fn 'something))
    (should
     (eq (fussy--should-propertize-p) nil)))

  ;; `fussy-fitler-fn' is `orderless'.
  (cl-letf* (((symbol-function 'fussy--using-pcm-highlight-p)
              (lambda () nil))
             (fussy-filter-fn 'fussy-filter-orderless)
             (fussy-propertize-fn 'something))
    (should
     (eq (fussy--should-propertize-p) nil)))

  ;; `fussy-propertize-fn' is nil.
  (cl-letf* (((symbol-function 'fussy--using-pcm-highlight-p)
              (lambda () nil))
             (fussy-filter-fn 'not-orderless)
             (fussy-propertize-fn nil))
    (should
     (eq (fussy--should-propertize-p) nil)))

  ;; Should return something.
  (cl-letf* (((symbol-function 'fussy--using-pcm-highlight-p)
              (lambda () nil))
             (fussy-filter-fn 'not-orderless)
             (fussy-propertize-fn 'something))
    (should
     (fussy--should-propertize-p))))

(ert-deftest fussy--using-pcm-highlight-p ()
  "Test `fussy--using-pcm-highlight-p' return correct values."
  ;; `fussy-score-fn' returns no indices.
  (let ((fussy-score-fn 'fn-without-indices)
        (fussy-score-fns-without-indices '(fn-without-indices))
        (fussy-filter-fn 'not-orderless))
    (should
     (fussy--using-pcm-highlight-p)))

  ;; `fussy-filter-fn' is using `orderless'.
  (let ((fussy-score-fn 'fn-without-indices)
        (fussy-score-fns-without-indices '(fn-without-indices))
        (fussy-filter-fn 'fussy-filter-orderless))
    (should
     (eq (fussy--using-pcm-highlight-p) nil))))
