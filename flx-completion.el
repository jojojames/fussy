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
length. e.g. The shortest length N words will be filtered to be scored.

2. Score only up to N words. The rest won't be scored.

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
  #'flx-propertize
  "Function used to propertize `flx' matches.

Takes OBJ \(to be propertized\) and
SCORE \(list of indices of OBJ to be propertized\).
If this is nil, don't propertize (e.g. highlight matches) at all."
  :type `(choice
          (const :tag "No highlighting" nil)
          (const :tag "By completions-common face."
                 ,#'flx-completion-propertize-by-completions-common)
          (const :tag "By flx propertization." ,#'flx-propertize)
          (function :tag "Custom function"))
  :group 'flx-completion)

(defun flx-completion--propertize (obj score)
  "Propertize OBJ with SCORE by calling `flx-completion-propertize-fn'."
  (unless (null flx-completion-propertize-fn)
    (funcall flx-completion-propertize-fn obj score)))

(defun flx-completion-propertize-by-completions-common (obj score)
  "Return propertized copy of OBJ according to score.

SCORE of nil means to clear the properties."
  (let ((block-started (cadr score))
        (last-char nil)
        (str (if (consp obj)
                 (substring-no-properties (car obj))
               (substring-no-properties obj))))
    (when score
      (dolist (char (cdr score))
        (when (and last-char
                   (not (= (1+ last-char) char)))
          (put-text-property block-started  (1+ last-char)
                             'face 'completions-common-part str)

          (setq block-started char))
        (setq last-char char))
      (put-text-property block-started  (1+ last-char)
                         'face 'completions-first-difference str))
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
  (let ((completion-ignore-case flx-completion-ignore-case))
    (pcase-let ((`(,all ,pattern ,prefix ,_suffix ,_carbounds)
                 (completion-substring--all-completions
                  string
                  table pred point
                  #'completion-flex--make-flex-pattern)))
      (when all
        (nconc
         (if (or (> (length string) flx-completion-max-query-length)
                 (string= string ""))
             ;; Copy of `completion-flex-all-completions' when we don't do any
             ;; sorting.
             (completion-pcm--hilit-commonality pattern all)
           (if (< (length all) flx-completion-max-candidate-limit)
               (flx-completion-score all string)
             (let ((unscored-candidates '())
                   (candidates-to-score '()))
               ;; Pre-sort the candidates by length before partitioning.
               (setq unscored-candidates
                     (sort all (lambda (c1 c2)
                                 (< (length c1)
                                    (length c2)))))
               ;; Partition the candidates into sorted and unsorted groups.
               (dotimes (_n (min (length unscored-candidates)
                                 flx-completion-max-candidate-limit))
                 (push (pop unscored-candidates) candidates-to-score))
               (append
                ;; Compute all of the flx scores only for cands-to-sort.
                (flx-completion-score (reverse candidates-to-score) string)
                ;; Add the unsorted candidates.
                unscored-candidates))))
         (length prefix))))))

(defun flx-completion-score (candidates string)
  "Score and propertize CANDIDATES using STRING."
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
         (put-text-property 0 1 'completion-score
                            (car score)
                            x)
         (setq x (flx-completion--propertize
                  x score)))))
     x)
   candidates))

;;;###autoload
(progn
  (put 'flx 'completion--adjust-metadata #'completion--flex-adjust-metadata)
  (add-to-list 'completion-styles-alist
               '(flx flx-completion-try-completions flx-completion-all-completions
                     "Flx Fuzzy completion.")))

(provide 'flx-completion)
;;; flx-completion.el ends here
