;;; flx-completion.el --- Fuzzy completion style using `flx' -*- lexical-binding: t; -*-

;; Copyright 2022 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (flx "0.5"))
;; Keywords: matching
;; Homepage: https://github.com/jojojames/flx-completion

;;; Commentary:

;; This is a fuzzy Emacs completion style similar to the built-in
;; `flex' style, but using `flx' for scoring.

;; To use this style, prepend `flx' to `completion-styles'.
;; To speed up `flx' matching, use https://github.com/jcs-elpa/flx-rs.

(require 'flx)

;;; Code:

(defgroup flx-completion nil
  "Fuzzy completion style using `flx.'."
  :group 'flx
  :link '(url-link :tag "GitHub" "https://github.com/jojojames/flx-completion"))

(defcustom flx-completion-max-query-length 128
  "Collections with queries longer than this are not scored using `flx'.

See `flx-completion-all-completions' for implementation details."
  :group 'flx-completion
  :type 'integer)

(defcustom flx-completion-max-candidate-limit 1000
  "Apply optimizations for collections greater than this limit.

`flx-completion-all-completions' will apply some optimizations.

N -> this variable's value

1. The collection (to be scored) will initially be filtered based off word
length.  e.g. The shortest length N words will be filtered to be scored.

2. Score only up to N words.  The rest won't be scored.

Additional implementation details:
https://github.com/abo-abo/swiper/issues/207#issuecomment-141541960"
  :group 'flx-completion
  :type 'integer)

(defcustom flx-completion-ignore-case t
  "If t, `flx' ignores `completion-ignore-case'."
  :group 'flx-completion
  :type 'boolean)

(defcustom flx-completion-max-word-length-to-score 1000
  "Words that are longer than this length are not scored by flx."
  :group 'flx-completion
  :type 'integer)

(defcustom flx-completion-propertize-fn
  #'flx-completion--propertize-common-part
  "Function used to propertize `flx' matches.

Takes OBJ \(to be propertized\) and
SCORE \(list of indices of OBJ to be propertized\).
If this is nil, don't propertize (e.g. highlight matches) at all."
  :type `(choice
          (const :tag "No highlighting" nil)
          (const :tag "By completions-common face."
                 ,#'flx-completion--propertize-common-part)
          (const :tag "By flx propertization." ,#'flx-propertize)
          (function :tag "Custom function"))
  :group 'flx-completion)

;;;###autoload
(defcustom flx-completion-adjust-metadata-fn
  #'flx-completion--adjust-metadata
  "Used for `completion--adjust-metadata' to adjust completion metadata.

`completion--adjust-metadata' is what is used to set up sorting of candidates
based on `completion-score'.  The default `flex' completion style in
`completion-styles' uses `completion--flex-adjust-metadata' which respects
the original completion table's sort functions:

  e.g. display-sort-function, cycle-sort-function

The default of `flx-completion-adjust-metadata-fn' is used instead to ignore
existing sort functions in favor of sorting based only on `flx' match scores."
  :type `(choice
          (const :tag "Adjust metadata using flx."
                 ,#'flx-completion--adjust-metadata)
          (const :tag "Adjust metadata using flex."
                 ,#'completion--flex-adjust-metadata)
          (function :tag "Custom function"))
  :group 'flx-completion)

(defcustom flx-completion-compare-same-score-fn
  #'flx-completion--strlen<
  "Function used to compare matches with the same 'completion-score.

FN takes in and compares two candidate strings C1 and C2 and
returns which candidates should have precedence.

If this is nil, do nothing."
  :type `(choice
          (const :tag "Don't compare candidates with same score." nil)
          (const :tag "Shorter candidates have precedence."
                 ,#'flx-completion--strlen<)
          (const :tag "Longer candidates have precedence."
                 ,#'flx-completion--strlen>)
          (function :tag "Custom function"))
  :group 'flx-completion)

(defcustom flx-completion-max-limit-preferred-candidate-fn
  #'flx-completion--strlen<
  "Function used when collection length is greater than\

`flx-completion-max-candidate-limit'.

FN takes in and compares two candidate strings C1 and C2 and
returns which candidates should have precedence.

If this is nil, take the first `flx-completion-max-candidate-limit' number
of candidates that was returned by the completion table."
  :type `(choice
          (const :tag "Take the first X number of candidates." nil)
          (const :tag "Shorter candidates have precedence."
                 ,#'flx-completion--strlen<)
          (const :tag "Longer candidates have precedence."
                 ,#'flx-completion--strlen>)
          (function :tag "Custom function"))
  :group 'flx-completion)

(defcustom flx-completion-filter-fn
  #'flx-completion-filter-like-flex
  "Function used for filtering candidates before scoring with `flx'.

FN takes in the same arguments as `flx-completion-try-completions'.

This FN should not be nil.

Use `flx-completion-filter-using-orderless' for faster filtering through the
`all-completions' (written in C) interface."
  :type `(choice
          (const :tag "Built in Flex Filtering"
                 ,#'flx-completion-filter-like-flex)
          (const :tag "Orderless Filtering"
                 ,#'flx-completion-filter-using-orderless)
          (function :tag "Custom function"))
  :group 'flx-completion)

(defmacro flx-completion--measure-time (&rest body)
  "Measure the time it takes to evaluate BODY.
https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00087.html"
  `(let ((time (current-time)))
     (let ((result ,@body))
       (message "%.06f" (float-time (time-since time)))
       result)))

(defun flx-completion--propertize-common-part (obj score)
  "Return propertized copy of OBJ according to score.

SCORE of nil means to clear the properties."
  (let ((block-started (cadr score))
        (last-char nil)
        ;; Originally we used `substring-no-properties' when setting str but
        ;; that strips text properties that other packages may set.
        ;; One example is `consult', which sprinkles text properties onto
        ;; the candidate. e.g. `consult--line-prefix' will check for
        ;; 'consult-location on str candidate.
        (str (if (consp obj) (car obj) obj)))
    (when score
      (dolist (char (cdr score))
        (when (and last-char
                   (not (= (1+ last-char) char)))
          (add-face-text-property block-started (1+ last-char)
                                  'completions-common-part nil str)
          (setq block-started char))
        (setq last-char char))
      (add-face-text-property block-started (1+ last-char)
                              'completions-common-part nil str)
      (when (and
             last-char
             (> (length str) (+ 2 last-char)))
        (add-face-text-property (1+ last-char) (+ 2 last-char)
                                'completions-first-difference
                                nil
                                str)))
    (if (consp obj)
        (cons str (cdr obj))
      str)))

(defun flx-completion-try-completions (string table pred point)
  "Try to flex-complete STRING in TABLE given PRED and POINT.

Implement `try-completions' interface by using `completion-flex-try-completion'."
  (completion-flex-try-completion string table pred point))

(defun flx-completion-all-completions (string table pred point)
  "Get flex-completions of STRING in TABLE, given PRED and POINT.

Implement `all-completions' interface by using `flx' scoring."
  (pcase-let* ((completion-ignore-case flx-completion-ignore-case)
               (using-pcm-highlight (eq table 'completion-file-name-table))
               (`(,all ,pattern ,prefix)
                (funcall flx-completion-filter-fn
                         string table pred point)))
    (when all
      (nconc
       (if (or (> (length string) flx-completion-max-query-length)
               (string= string ""))
           (flx-completion--maybe-highlight pattern all :always-highlight)
         (if (< (length all) flx-completion-max-candidate-limit)
             (flx-completion--maybe-highlight
              pattern
              (flx-completion--score all string using-pcm-highlight)
              using-pcm-highlight)
           (let ((unscored-candidates '())
                 (candidates-to-score '()))
             ;; Pre-sort the candidates by length before partitioning.
             (setq unscored-candidates
                   (if flx-completion-max-limit-preferred-candidate-fn
                       (sort
                        all flx-completion-max-limit-preferred-candidate-fn)
                     ;; If `flx-completion-max-limit-preferred-candidate-fn'
                     ;; is nil, we'll partition the candidates as is.
                     all))
             ;; Partition the candidates into sorted and unsorted groups.
             (dotimes (_n (min (length unscored-candidates)
                               flx-completion-max-candidate-limit))
               (push (pop unscored-candidates) candidates-to-score))
             (append
              ;; Compute all of the flx scores only for cands-to-sort.
              (flx-completion--maybe-highlight
               pattern
               (flx-completion--score
                (reverse candidates-to-score) string using-pcm-highlight)
               using-pcm-highlight)
              ;; Add the unsorted candidates.
              ;; We could highlight these too,
              ;; (e.g. with `flx-completion--maybe-highlight') but these are
              ;; at the bottom of the pile of candidates.
              unscored-candidates))))
       (length prefix)))))

(defun flx-completion--score (candidates string &optional using-pcm-highlight)
  "Score and propertize \(if not USING-PCM-HIGHLIGHT\) CANDIDATES using STRING."
  (mapcar
   (lambda (x)
     (setq x (copy-sequence x))
     (cond
      ((> (length x) flx-completion-max-word-length-to-score)
       (put-text-property 0 1 'completion-score 0 x))
      (:default
       (let ((score (if (fboundp 'flx-rs-score)
                        (flx-rs-score x string)
                      (flx-score x string flx-strings-cache))))
         ;; This is later used by `completion--adjust-metadata' for sorting.
         (put-text-property 0 1 'completion-score
                            (car score)
                            x)
         ;; If we're using pcm highlight, we don't need to propertize the
         ;; string here. This is faster than the pcm highlight but doesn't
         ;; seem to work with `find-file'.
         (unless (or using-pcm-highlight
                     (flx-completion-using-orderless-p)
                     (null flx-completion-propertize-fn))
           (setq
            x (funcall flx-completion-propertize-fn x score))))))
     x)
   candidates))

(defun flx-completion--maybe-highlight (pattern collection using-pcm-highlight)
  "Highlight COLLECTION using PATTERN if USING-PCM-HIGHLIGHT is true."
  (if (and using-pcm-highlight
           (not (flx-completion-using-orderless-p)))
      ;; This seems to be the best way to get highlighting to work consistently
      ;; with `find-file'.
      (completion-pcm--hilit-commonality pattern collection)
    ;; This will be the case when the `completing-read' function is not
    ;; `find-file'.
    ;; Assume that the collection has already been highlighted.
    ;; e.g. When `using-pcm-highlight' is nil or we're using `orderless' for
    ;; filtering and highlighting.
    collection))

;;;###autoload
(progn
  (put 'flx 'completion--adjust-metadata flx-completion-adjust-metadata-fn)
  (add-to-list 'completion-styles-alist
               '(flx flx-completion-try-completions flx-completion-all-completions
                     "Flx Fuzzy completion.")))

(defun flx-completion--adjust-metadata (metadata)
  "If `flx' is actually doing filtering, adjust METADATA's sorting."
  (let ((flex-is-filtering-p
         ;; JT@2019-12-23: FIXME: this is kinda wrong.  What we need
         ;; to test here is "some input that actually leads/led to
         ;; flex filtering", not "something after the minibuffer
         ;; prompt".  E.g. The latter is always true for file
         ;; searches, meaning we'll be doing extra work when we
         ;; needn't.
         (or (not (window-minibuffer-p))
             (> (point-max) (minibuffer-prompt-end)))))
    `(metadata
      ,@(and flex-is-filtering-p
             `((display-sort-function . flx-completion--sort)))
      ,@(and flex-is-filtering-p
             `((cycle-sort-function . flx-completion--sort)))
      ,@(cdr metadata))))

(defun flx-completion--sort (completions)
  "Sort COMPLETIONS using `completion-score' and completion length."
  (sort
   completions
   (lambda (c1 c2)
     (let ((s1 (or (get-text-property 0 'completion-score c1) 0))
           (s2 (or (get-text-property 0 'completion-score c2) 0)))
       (if (and (= s1 s2)
                flx-completion-compare-same-score-fn)
           (funcall flx-completion-compare-same-score-fn c1 c2)
         ;; Candidates with higher completion score have precedence.
         (> s1 s2))))))

(defun flx-completion--strlen< (c1 c2)
  "Return t if C1's length is less than C2's length."
  (< (length c1) (length c2)))

(defun flx-completion--strlen> (c1 c2)
  "Return t if C1's length is greater than C2's length."
  (> (length c1) (length c2)))

(defun flx-completion-using-orderless-p ()
  "Return whether or not we're using `orderless' for filtering."
  (eq flx-completion-filter-fn 'flx-completion-filter-using-orderless))

;; Filtering functions.

(declare-function "orderless-filter" "orderless")
(declare-function "orderless-highlight-matches" "orderless")
(declare-function "orderless--prefix+pattern" "orderless")
(defvar orderless-skip-highlighting)

(defun flx-completion-filter-using-orderless (string table pred _point)
  "Match STRING to the entries in TABLE.

Respect PRED and POINT.  Use `orderless' for filtering."
  (pcase-let* ((orderless-matching-styles '(orderless-flex))
               (completions (orderless-filter string table pred))
               (`(,prefix . ,pattern)
                (orderless--prefix+pattern string table pred))
               (skip-highlighting
                (if (functionp orderless-skip-highlighting)
                    (funcall orderless-skip-highlighting)
                  orderless-skip-highlighting)))
    (if skip-highlighting
        (list completions pattern prefix)
      (list (orderless-highlight-matches pattern completions) pattern prefix))))

(defun flx-completion-filter-like-flex (string table pred point)
  "Match STRING to the entries in TABLE.

Respect PRED and POINT.  The filter here is the same as in
`completion-flex-all-completions'."
  (pcase-let ((`(,completions ,pattern ,prefix ,_suffix ,_carbounds)
               (completion-substring--all-completions
                string
                table pred point
                #'completion-flex--make-flex-pattern)))
    (list completions pattern prefix)))

;; Integrations

;; `company' integration.
(defvar company-backend)
;; Use with `company-transformers'.
;; (setq company-transformers
;;           '(flx-completion--company-sort-by-completion-score))
(defun flx-completion--company-sort-by-completion-score (candidates)
  "`company' transformer to sort CANDIDATES."
  (if (functionp company-backend)
      candidates
    (flx-completion--sort candidates)))

(provide 'flx-completion)
;;; flx-completion.el ends here
