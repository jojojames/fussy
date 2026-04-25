;;; fussy-test.el --- `fussy' test. -*- lexical-binding: t; -*-
(require 'ert)
(require 'fussy)

;; For `help--symbol-completion-table'.
(require 'help-fns)

;; `fzf-native' provides the C module exercised by the multibyte tests
;; below. It is an optional dev dependency in `Eask' (gated on platforms
;; with shipped binaries), so its presence is not guaranteed. We try to
;; load both the package and its dynamic module; on any failure we set
;; `fussy-test--fzf-native-available-p' to nil and the multibyte tests
;; will skip rather than fail. This keeps the suite green on platforms
;; without a prebuilt module and tolerates module/Emacs ABI mismatches.
(defconst fussy-test--fzf-native-available-p
  (condition-case _
      (progn
        (require 'fzf-native)
        (when (fboundp 'fzf-native-load-dyn)
          (fzf-native-load-dyn))
        (fboundp 'fzf-native-score-all))
    (error nil))
  "Non-nil if `fzf-native' loaded and its module is callable.")

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
;; (@* "`fussy-filter-default'" )
;;

(ert-deftest fussy-filter-default-in-all-completions-perf-test ()
  "Assert `fussy-filter-default' with is the fastest filter method.

Called from `fussy-all-completions'."
  (dolist (query '("a" "b" "c"))
    (let* ((table 'help--symbol-completion-table)
           (pred nil)
           (point 1)
           (fussy-default-regex-fn 'fussy-pattern-flex-1)
           (fussy-filter-fn 'fussy-filter-default)
           (fussy-prefer-prefix nil)
           (fussy-use-cache nil)
           (default-res
            (car
             (benchmark-run 10
               (fussy-all-completions query table pred point)))))
      (should
       (< default-res
          (let ((fussy-filter-fn 'fussy-filter-flex))
            (car (benchmark-run 10
                   (fussy-all-completions query table pred point))))))
      (should
       (< default-res
          (let ((fussy-filter-fn 'fussy-filter-orderless))
            (car (benchmark-run 10
                   (fussy-all-completions query table pred point)))))))))

(ert-deftest fussy-filter-fn-default-perf-test ()
  "Assert `fussy-filter-default' is the fastest filter method."
  (dolist (query '("a" "b" "c" "def"))
    (let* ((table 'help--symbol-completion-table)
           (pred nil)
           (point 1)
           (fussy-prefer-prefix nil)
           (fussy-default-regex-fn 'fussy-pattern-flex-1)
           (default-res
            (car (benchmark-run 3
                   (fussy-filter-default query table pred point)))))
      (should
       (<
        default-res
        (car (benchmark-run 3
               (fussy-filter-orderless query table pred point)))))
      (should
       (<
        default-res
        (car (benchmark-run 3
               (fussy-filter-flex query table pred point))))))))

(ert-deftest fussy-filter-fn-default-candidates ()
  "Assert result of `fussy-filter-default' matches other filters."
  (dolist (query '("a" "b" "c" "def"))
    (let* ((table 'help--symbol-completion-table)
           (pred nil)
           (point 1)
           (default-res (fussy-filter-default query table pred point)))
      (should
       (= (length default-res)
          (length (fussy-filter-flex query table pred point))))
      (should
       (= (length default-res)
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
     (< (get-text-property 0 'completion-score (nth 1 string-cache-res))
        (get-text-property 0 'completion-score (nth 0 string-cache-res))))

    ;; With `flx-file-cache', candidate 1 wins against candidate 2 which shows
    ;; `flx-file-cache' is more appropriate than `flx-strings-cache' for
    ;; filenames.
    (should
     (> (get-text-property 0 'completion-score (nth 1 file-cache-res))
        (get-text-property 0 'completion-score (nth 0 file-cache-res))))))

(ert-deftest fussy-score-input-test ()
  "Test various[TODO] inputs with `fussy-score'."
  (let* ((fussy-score-fn (lambda (str query &optional cache)
                           (list nil)))
         (candidates '("a" "b")))
    (should
     (not (fussy-score candidates "blah")))))

;;
;; (@* "`fussy-propertize-common-part'" )
;;

(ert-deftest fussy-propertize-common-part-test ()
  "Test `fussy-propertize-common-part'."
  (should
   (fussy-propertize-common-part "^" '(0)))
  (should
   (fussy-propertize-common-part "^" '()))
  (should
   (fussy-propertize-common-part "^" nil))
  (should
   (fussy-propertize-common-part "abc" '(1 2)))
  (should
   (fussy-propertize-common-part "abcd" '(1 3))))

(ert-deftest fussy-propertize-common-part-bad-indices ()
  "Test `fussy-propertize-common-part' handles bad indices returned."
  (should
   (fussy-propertize-common-part "abcd" '(1 4)))
  (should
   (fussy-propertize-common-part "abcd" '(1 199)))
  (should
   (fussy-propertize-common-part "abcd" '(1 3 4))))

;;
;; (@* "`fussy--should-propertize-p'" )
;;

(ert-deftest fussy--should-propertize-p ()
  "Test `fussy--should-propertize-p' return correct values."
  ;; use-pcm-highlight is t.
  (cl-letf* (((symbol-function 'fussy--use-pcm-highlight-p)
              (lambda () t))
             (fussy-filter-fn 'not-orderless)
             (fussy-propertize-fn 'something))
    (should
     (eq (fussy--should-propertize-p) nil)))

  ;; `fussy-fitler-fn' is `orderless'.
  (cl-letf* (((symbol-function 'fussy--use-pcm-highlight-p)
              (lambda () nil))
             (fussy-filter-fn 'fussy-filter-orderless)
             (fussy-propertize-fn 'something))
    (should
     (eq (fussy--should-propertize-p) nil)))

  ;; `fussy-propertize-fn' is nil.
  (cl-letf* (((symbol-function 'fussy--use-pcm-highlight-p)
              (lambda () nil))
             (fussy-filter-fn 'not-orderless)
             (fussy-propertize-fn nil))
    (should
     (eq (fussy--should-propertize-p) nil)))

  ;; Should return something.
  (cl-letf* (((symbol-function 'fussy--use-pcm-highlight-p)
              (lambda () nil))
             (fussy-filter-fn 'not-orderless)
             (fussy-propertize-fn 'something))
    (should
     (fussy--should-propertize-p))))

;;
;; (@* "`fussy--use-pcm-highlight-p'" )
;;

(ert-deftest fussy--use-pcm-highlight-p ()
  "Test `fussy--use-pcm-highlight-p' return correct values."
  ;; `fussy-score-fn' returns no indices.
  (let ((fussy-score-ALL-fn 'fussy-score)
        (fussy-score-fn 'fn-without-indices)
        (fussy-score-fns-without-indices '(fn-without-indices))
        (fussy-filter-fn 'not-orderless))
    (should
     (fussy--use-pcm-highlight-p)))

  ;; `fussy-filter-fn' is using `orderless'.
  (let ((fussy-score-ALL-fn 'fussy-score)
        (fussy-score-fn 'fn-without-indices)
        (fussy-score-fns-without-indices '(fn-without-indices))
        (fussy-filter-fn 'fussy-filter-orderless))
    (should
     (eq (fussy--use-pcm-highlight-p) nil))))

;;
;; (@* "`fussy--history-hash-table'" )
;;

(ert-deftest fussy--history-hash-table-test ()
  "Test `fussy--history-hash-table'."
  (setf fussy-history-variable '("first" "second" "first"))
  (let ((minibuffer-history-variable 'fussy-history-variable))
    (let ((hist (fussy--history-hash-table)))
      (should (eq (gethash "first" hist) 0))
      (should (eq (gethash "second" hist) 1)))))

;;
;; (@* "Multibyte" )
;;
;; These exercise the fussy-level integration with the fzf-native C module
;; against invalid unibyte and legitimate multibyte candidates. The C-layer
;; handling lives in `fzf-native-test.el'; here we make sure fussy's wrappers
;; (`fussy-fzf-score' via `fussy-score-ALL-fn', and the per-candidate
;; `fussy-fzf-native-score' path via `fussy-score') don't signal and return
;; useful results.

(defconst fussy-test--bad-bytes
  (string-as-multibyte ";; Copyright 2022 Jo Be�����")
  "The exact reproducer from the original bug report.")

(ert-deftest fussy-score-FN-all-multibyte-test ()
  "`fussy-fzf-score' on a byte-junk candidate does not signal."
  (skip-unless fussy-test--fzf-native-available-p)
  (let* ((fussy-score-ALL-fn 'fussy-fzf-score)
         (result (funcall fussy-score-ALL-fn
                          (list fussy-test--bad-bytes)
                          "C")))
    ;; The bad-byte string contains "C" (in "Copyright"); after
    ;; encode-coding-string coercion in the C module, fzf scores it
    ;; normally, so the candidate survives in the result.
    (should (member fussy-test--bad-bytes result))))

(ert-deftest fussy-score-FN-all-multibyte-mixed-batch-test ()
  "A bad candidate does not prevent scoring of good ones in the same batch.
The bad candidate goes through `encode-coding-string' coercion in the C
module and is scored normally; it survives in the result alongside the
clean matches."
  (skip-unless fussy-test--fzf-native-available-p)
  (let* ((fussy-score-ALL-fn 'fussy-fzf-score)
         (candidates (list "CCCCC" fussy-test--bad-bytes "CC"))
         (result (funcall fussy-score-ALL-fn candidates "C")))
    (should (member "CCCCC" result))
    (should (member "CC" result))
    (should (member fussy-test--bad-bytes result))))

(ert-deftest fussy-score-FN-all-chinese-test ()
  "`fussy-fzf-score' handles Chinese candidates against a Chinese query."
  (skip-unless fussy-test--fzf-native-available-p)
  (let* ((fussy-score-ALL-fn 'fussy-fzf-score)
         (result (funcall fussy-score-ALL-fn
                          '("你好世界" "Hello" "你是")
                          "你")))
    (should (member "你好世界" result))
    (should (member "你是" result))))

(ert-deftest fussy-score-multibyte-test ()
  "`fussy-score' with `fussy-fzf-native-score' does not signal on bad bytes."
  (skip-unless fussy-test--fzf-native-available-p)
  (let* ((fussy-score-fn 'fussy-fzf-native-score)
         ;; Ensure the C module itself is what's being exercised, not the
         ;; Elisp-side bad-char stripping.
         (fussy-remove-bad-char-fn nil)
         (result (fussy-score (list fussy-test--bad-bytes) "C")))
    ;; bad-bytes contains "C" (in "Copyright"); coercion lets fzf score it,
    ;; so the candidate survives.
    (should (member fussy-test--bad-bytes result))))

;;
;; (@* "Util" )
;;

(defun fussy-pattern-str (pattern-compiled)
  "Return PATTERN-COMPILED as a string.

PATTERN-COMPILED is a list so we want to check all strings inside it."
  (format "%S" pattern-compiled))
