;;; fussy-test.el --- `fussy' test. -*- lexical-binding: t; -*-
(require 'ert)
(require 'fussy)

;; For `help--symbol-completion-table'.
(require 'help-fns)

;; These tests garbage collect alot so try to avoid garbage collection since
;; we're doing benchmark tests.
(setf gc-cons-threshold most-positive-fixnum
      garbage-collection-messages t)

(defconst fussy--consult--tofu-char #x200000
  "Special character used to encode line prefixes for disambiguation.
We use invalid characters outside the Unicode range.

This is a copy of `consult--tofu-char' that we've copied over here so that we
can strip out this character for `consult' specific functions that encode
the character into the candidate.

See `fussy-without-tofu-char'.")

(defvar fussy-history-variable '())

;;
;; (@* "`fussy-strlen<'" )
;;

(ert-deftest fussy-strlen<-test ()
  (should (equal nil (fussy-strlen< "abc" "a")))
  (should (equal t (fussy-strlen< "a" "bc")))
  (should (equal nil (fussy-strlen< "a" "a"))))

;;
;; (@* "`fussy-strlen>'" )
;;

(ert-deftest fussy-strlen>-test ()
  (should (equal t (fussy-strlen> "abc" "a")))
  (should (equal nil (fussy-strlen> "a" "bc")))
  (should (equal nil (fussy-strlen> "a" "a"))))


;;
;; (@* "`fussy-histlen<'" )
;;

(ert-deftest fussy-histlen<-test ()
  (setf fussy-history-variable '("first" "second"))
  (let ((minibuffer-history-variable 'fussy-history-variable))
    (setf fussy--hist-hash (fussy--history-hash-table))
    (should (equal t (fussy-histlen< "first" "second")))
    (should (equal nil (fussy-histlen< "second" "first")))
    (should (equal nil (fussy-histlen< "doesntexist" "first")))
    (should (equal t (fussy-histlen< "second" "doesntexist")))
    (should (equal nil (fussy-histlen< "doesntexist" "doesntexist")))))

(defun fussy-histlen<-no-cache (c1 c2)
  "Return t if C1 occurred more recently than C2.

Check C1 and C2 in `minibuffer-history-variable'."
  (let* ((hist (and (not (eq minibuffer-history-variable t))
                    (symbol-value minibuffer-history-variable))))
    (catch 'found
      (dolist (h hist)
        (when (string= c1 h)
          (throw 'found t))
        (when (string= c2 h)
          (throw 'found nil))))))

(defvar-local fussy-histlen-test-history-variable
  '("first" "second" "third" "four" "fives" "dfljh" "90909" "23232" "zxj"
    "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "fzf" "zbz" "fsd" "2df" "fzm"
    "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "123" "345" "333" "4rf" "ffz"
    "u" "v" "w" "x" "y" "z" "ab" "abc" "cd" "fdf" "ffff" "bzbzb" "abcdef"
    "us" "av" "dw" "fx" "xy" "az" "ab" "abc" "zyy" "ffffsf" "zvvv" "fjkl"
    "aus" "adv" "ddw" "afx" "sxy" "ty" "turur" "fskdff" "sso" "jaj" "bbb"
    "faz" "zb" "abc" "cfsd" "jo" "be" "ja" "mssss" "ugn" "ney" "gon" "is"
    "fza" "zb" "acb" "cfsd" "oj" "eb" "ja" "smsss" "gnu" "nye" "ogn" "ls"
    "marp" "guts" "onepi" "looifi" "zoloooro" "namiisea" "lofurobi" "nsj"
    "san" "jiiee" "mrco" "lpira" "gte" "asdf" "fsdfksjk" "fsfsf" "bcsfsa"
    "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "asdfs"))

(ert-deftest fussy-histlen<-benchmark-test ()
  "Assert that as the size of the collection grows, the cache is faster."
  (let ((minibuffer-history-variable 'fussy-histlen-test-history-variable))
    (setf fussy--hist-hash (fussy--history-hash-table))
    (should
     (<
      (car (benchmark-run 10000
             (fussy-histlen< "xyz" "abc")))
      (car (benchmark-run 10000
             (fussy-histlen<-no-cache "xyz" "abc")))))))

;; (@* "`fussy-histlen->strlen<'" )

(ert-deftest fussy-histlen->strlen< ()
  (setf fussy-history-variable '("first" "second"))
  (let ((minibuffer-history-variable 'fussy-history-variable))
    (should (equal t (fussy-histlen->strlen< "first" "second")))
    (should (equal nil (fussy-histlen->strlen< "second" "first")))
    (should (equal nil (fussy-histlen->strlen< "doesntexist" "first")))
    (should (equal t (fussy-histlen->strlen< "second" "doesntexist")))
    (should
     (equal nil (fussy-histlen->strlen< "doesntexist" "doesntexist")))
    (should (equal nil (fussy-histlen->strlen< "longerstring" "short")))
    (should (equal t (fussy-histlen->strlen< "short" "longerstring")))))

(defun fussy-histlen->strlen<-no-cache (c1 c2)
  "Return t if C1 occurs more recently than C2 or is shorter than C2."
  (let* ((hist (and (not (eq minibuffer-history-variable t))
                    (symbol-value minibuffer-history-variable))))
    (let ((result (catch 'found
                    (dolist (h hist)
                      (when (string= c1 h)
                        (throw 'found 'c1))
                      (when (string= c2 h)
                        (throw 'found 'c2))))))
      (if result
          (eq result 'c1)
        (fussy-strlen< c1 c2)))))

(ert-deftest fussy-histlen->strlen<-benchmark-test ()
  "Assert that as the size of the collection grows, the cache is faster."
  (let ((minibuffer-history-variable 'fussy-histlen-test-history-variable))
    (setf fussy--hist-hash (fussy--history-hash-table))
    (should
     (<
      (car (benchmark-run 10000
             (fussy-histlen->strlen< "twelve" "eleven")))
      (car (benchmark-run 10000
             (fussy-histlen->strlen<-no-cache "twelve" "eleven")))))))

;;
;; (@* "`fussy-filter-fast'" )
;;

(ert-deftest fussy-filter-fast-in-all-completions-perf-test ()
  "Assert `fussy-filter-fast' with is the fastest filter method.

Called from `fussy-all-completions'."
  (dolist (query '("a" "b" "c"))
    (let* ((table 'help--symbol-completion-table)
           (pred nil)
           (point 1)
           (fussy-fast-regex-fn 'fussy-pattern-flex-1)
           (fussy-filter-fn 'fussy-filter-fast)
           (fussy-prefer-prefix nil)
           (fast-res
            (car
             (benchmark-run 10
               (fussy-all-completions query table pred point)))))
      (should
       (< fast-res
          (let ((fussy-filter-fn 'fussy-filter-flex))
            (car (benchmark-run 10
                   (fussy-all-completions query table pred point))))))
      (should
       (< fast-res
          (let ((fussy-filter-fn 'fussy-filter-orderless))
            (car (benchmark-run 10
                   (fussy-all-completions query table pred point)))))))))

(ert-deftest fussy-filter-fn-fast-perf-test ()
  "Assert `fussy-filter-fast' is the fastest filter method."
  (dolist (query '("a" "b" "c" "def"))
    (let* ((table 'help--symbol-completion-table)
           (pred nil)
           (point 1)
           (fussy-prefer-prefix nil)
           (fussy-fast-regex-fn 'fussy-pattern-flex-1)
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

;;
;; (@* "`fussy-score'" )
;;

(ert-deftest fussy-score-cache-test ()
  "Test that file cache makes a difference."
  (let*
      ((fussy-score-fn 'flx-score)
       (candidates
        '("~/.emacs.d/straight/repos/orderless/orderless.el"
          "~/Code/yyoshereios/iOSTest/yyosHereiPadRootViewController.h"))
       (string-cache-res
        (fussy-score candidates "odor" flx-strings-cache))
       (file-cache-res
        (fussy-score candidates "odor" flx-file-cache)))

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

;;
;; (@* "`fussy-without-unencodeable-chars'" )
;;

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

;;
;; (@* "`fussy-without-tofu-char'" )
;;

(ert-deftest fussy-without-tofu-char-test ()
  "Test `fussy-without-tofu-char'."
  ;; Good input.
  (should
   (string=
    (fussy-without-tofu-char "bb") "bb"))
  (should
   (string=
    (fussy-without-tofu-char "jj") "jj"))
  ;; Bad input.
  (should
   (string=
    (fussy-without-tofu-char (string-make-multibyte "Makefile"))
    "Makefile"))
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

(ert-deftest fussy-without-tofu-char-perf-test ()
  "Test `fussy-without-tofu-char' performance.

This test asserts `fussy-without-tofu-char''s speed."
  (let* ((tofu (char-to-string fussy--consult--tofu-char))
         (string-1 (concat "ojjjojjjoojjjjjoojjbb" tofu))
         (string-2 (string-as-multibyte ";; Copyright 2022 Jo Beøˆ€’"))
         (string-3 (string-as-multibyte ";; This buffer is for text that is not saved, and for Lisp evaluation.øˆ€€"))
         (performance-factor 50)
         ;; Feels like there is some cold/start warm start that may affect the test here.
         (_ (fussy-without-tofu-char string-1))
         (_ (fussy-without-tofu-char string-2))
         (_ (fussy-without-tofu-char string-3))
         (result-1 (car (benchmark-run 1000 (fussy-without-tofu-char string-1))))
         (result-2 (car (benchmark-run 1000 (fussy-without-tofu-char string-2))))
         (result-3 (car (benchmark-run 1000 (fussy-without-tofu-char string-3)))))
    (should
     (<
      (* performance-factor result-1)
      (car (benchmark-run 1000 (fussy-without-unencodeable-chars string-1)))))
    (should
     (<
      (* performance-factor result-2)
      (car (benchmark-run 1000 (fussy-without-unencodeable-chars string-2)))))
    (should
     (<
      (* performance-factor result-3)
      (car (benchmark-run 1000 (fussy-without-unencodeable-chars string-3)))))))

;;
;; (@* "`fussy-encode-coding-string'" )
;;

(ert-deftest fussy-encode-coding-string-test ()
  "Test `fussy-encode-coding-string'."
  ;; Good input.
  (should
   (string=
    (fussy-encode-coding-string "bb") "bb"))
  (should
   (string=
    (fussy-encode-coding-string "jj") "jj"))
  ;; Bad input.
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

(ert-deftest fussy-encode-coding-string-perf-test ()
  "Test `fussy-encode-coding-string' performance.

This test asserts `fussy-encode-coding-string' is much much faster than
`fussy-without-unencodeable-chars'."
  (let* ((tofu (char-to-string fussy--consult--tofu-char))
         (string-1 (concat "jjbb" tofu))
         (string-2 (string-as-multibyte ";; Copyright 2022 Jo Beøˆ€’"))
         (string-3 (string-as-multibyte ";; This buffer is for text that is not saved, and for Lisp evaluation.øˆ€€"))
         ;; Warm start?
         (_ (fussy-encode-coding-string string-1))
         (_ (fussy-encode-coding-string string-2))
         (_ (fussy-encode-coding-string string-3))
         (performance-factor 50))
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


;;
;; (@* "`fussy-propertize-common-part'" )
;;

(ert-deftest fussy-propertize-common-part-test ()
  "Test `fussy--propertize-common-part'."
  (should
   (fussy-propertize-common-part "^" '(0)))
  (should
   (fussy-propertize-common-part "^" '()))
  (should
   (fussy-propertize-common-part "^" nil))
  (should
   (fussy-propertize-common-part "abc" '(1 2))))

;;
;; (@* "`fussy--should-propertize-p'" )
;;

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

;;
;; (@* "`fussy--using-pcm-highlight-p'" )
;;

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

;;
;; (@* "`fussy-pattern-flex-2-test'" )
;;

(ert-deftest fussy-pattern-flex-2-test ()
  "Test flex-2 matches flex-rx and `orderless-filter'+`orderless-flex'."
  (should
   (string= (fussy-pattern-str (fussy-pattern-flex-2 "a"))
            (fussy-pattern-str (fussy-pattern-flex-rx "a"))))
  (should
   (string= (fussy-pattern-str (fussy-pattern-flex-2 "abc"))
            (fussy-pattern-str (fussy-pattern-flex-rx "abc"))))
  (should
   (string= (fussy-pattern-str (fussy-pattern-flex-2 "abasd90803423c"))
            (fussy-pattern-str (fussy-pattern-flex-rx "abasd90803423c"))))
  (let ((orderless-matching-styles '(orderless-flex)))
    (should
     (string= (fussy-pattern-str (fussy-pattern-flex-2 "a"))
              (fussy-pattern-str (orderless-pattern-compiler "a"))))
    (should
     (string= (fussy-pattern-str (fussy-pattern-flex-2 "abc"))
              (fussy-pattern-str (orderless-pattern-compiler "abc"))))
    (should
     (string= (fussy-pattern-str (fussy-pattern-flex-2 "abasd90803423c"))
              (fussy-pattern-str (orderless-pattern-compiler "abasd90803423c"))))
    (should
     (string= (fussy-pattern-str (fussy-pattern-flex-2 "reb"))
              (fussy-pattern-str (orderless-pattern-compiler "reb"))))))

;;
;; (@* "`fussy-pattern-flex-rx'" )
;;

(ert-deftest fussy-pattern-flex-rx-test ()
  "Test `fussy-pattern-flex-rx-test'.

Test that it matches the output of `orderless-filter'+`orderless-flex'."
  (should
   (string=
    (fussy-pattern-str (let ((orderless-matching-styles '(orderless-flex)))
                         (orderless-pattern-compiler "reb")))
    (fussy-pattern-str (fussy-pattern-flex-rx "reb"))))
  (should
   (string=
    (fussy-pattern-str (let ((orderless-matching-styles '(orderless-flex)))
                         (orderless-pattern-compiler "r")))
    (fussy-pattern-str (fussy-pattern-flex-rx "r"))))
  (should
   (string=
    (fussy-pattern-str (let ((orderless-matching-styles '(orderless-flex)))
                         (orderless-pattern-compiler "41234asdfbasdf..adf")))
    (fussy-pattern-str (fussy-pattern-flex-rx "41234asdfbasdf..adf")))))

(defun fussy-pattern-str (pattern-compiled)
  "Return PATTERN-COMPILED as a string.

PATTERN-COMPILED is a list so we want to check all strings inside it."
  (format "%S" pattern-compiled))


(ert-deftest fussy-fzf-native-score-test ()
  (unless (require 'fzf-native nil t)
    (ert-skip "fzf-native not installed"))
  (fzf-native-load-dyn)
  (should
   (equal (cdr (fussy-fzf-native-score "pockemon.txt" "txt"))
          '(9 10 11)))
  (should
   (equal (cdr (fussy-fzf-native-score "ãƒã‚±ãƒ¢ãƒ³.txt" "txt"))
          '(5 6 7))))
