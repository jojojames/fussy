;;; fussy.el --- Fuzzy completion style using `flx' and/or `fzf-native' -*- lexical-binding: t; -*-

;; Copyright 2022 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "28.2") (flx "0.5") (compat "30.0.0.0"))
;; Keywords: matching
;; Homepage: https://github.com/jojojames/fussy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a fuzzy Emacs completion style similar to the built-in
;; `flex' style, but using `flx' (or `fzf-native') for scoring.
;; It also supports various other fuzzy scoring systems in place of those two.

;; This package is intended to be used with packages that leverage
;; `completion-styles', e.g. `completing-read' and
;; `completion-at-point-functions'.

;; It is usable with `icomplete' (as well as `fido-mode'), `selectrum',
;; `vertico', `corfu', `helm' and `company-mode''s `company-capf'.

;; It is not currently usable with `ido' which doesn't support
;; `completion-styles' and has its own sorting and filtering system.  In
;; addition to those packages, other `company-mode' backends will not hook into
;; this package.  `ivy' support can be somewhat baked in following
;; https://github.com/jojojames/fussy#ivy-integration but the
;; performance gains may not be as high as the other `completion-read' APIs.

;; To use this style, prepend `fussy' to `completion-styles'.

;; For improved performance,`fussy-filter-fn' and `fussy-score-fn' for filtering
;; and scoring matches are good initial starting points for customization.

;; The various available scoring backends in `fussy-score-fn' have varying
;; levels of performance and match quality.
;; For a faster version that implements the same matching as `flx', use
;; https://github.com/jcs-elpa/flx-rs which is a native module written in Rust.

;; Other notable scoring backends supported by this package:
;; flx: https://github.com/lewang/flx
;; fzf: https://github.com/junegunn/fzf
;;   (e.g. https://github.com/dangduc/fzf-native)
;; skim: https://github.com/lotabout/fuzzy-matcher

;; For an exhaustive list of scoring backends, take a look at
;; https://github.com/jojojames/fussy#scoring-backends

(require 'flx)
(require 'compat)
(eval-when-compile (require 'subr-x))

;;; Code:

(declare-function "orderless-filter" "orderless")
(declare-function "orderless-highlight-matches" "orderless")
(declare-function "orderless--prefix+pattern" "orderless")
(defvar orderless-matching-styles)

;;
;; (@* "Landmarks" )
;;

;; `fussy-all-completions'
;; `fussy-score'
;; `fussy-filter-default'

;;
;; (@* "Customizations" )
;;

(defgroup fussy nil
  "Fuzzy completion style using `flx.'."
  :group 'flx
  :link '(url-link :tag "GitHub" "https://github.com/jojojames/fussy"))

(defcustom fussy-max-query-length 100
  "Collections with queries longer than this are not scored using `flx'.

See `fussy-all-completions' for implementation details."
  :group 'fussy
  :type 'integer)

(defcustom fussy-max-candidate-limit 30000
  "Apply optimizations for collections greater than this limit.

`fussy-all-completions' will apply some optimizations.

N -> this variable's value

1. The collection (to be scored) will initially be filtered based on
 `fussy-max-limit-preferred-candidate-fn'.

2. Score only up to N * `fussy-percent-of-candidates-to-score' words.
The rest won't be scored.

Additional implementation details:
https://github.com/abo-abo/swiper/issues/207#issuecomment-141541960"
  :group 'fussy
  :type 'integer)

(defcustom fussy-percent-of-candidates-to-score .7
  "When `fussy-max-candidate-limit' is hit, this variable determines the %
of candidates out of all candidates to score. For example, if
`fussy-max-candidate-limit' is 30000 and the collection is 40000, the # of
candidates to score will be 28000."
  :group 'fussy
  :type 'number)

(defcustom fussy-ignore-case t
  "If t, ignores `completion-ignore-case'.

If this is set to nil, highlighting may break for cases where we're
highlighting with `completion-pcm--hilit-commonality'."
  :group 'fussy
  :type 'boolean)

(defcustom fussy-score-threshold-to-filter nil
  "Candidates with scores of N or less are filtered.

Some backends such as `fussy-fuz-score' return negative scores
for low-quality matches.

If this is set to nil, threshold is defined by alist of
thresholds for score functions. Set this to a number to override
`fussy-score-threshold-to-filter-alist'.

Raise N to see fewer candidates. Lower N to see more
candidates. Keep N at 0 or more for performance."
  :group 'fussy
  :type 'integer)

(defcustom fussy-score-threshold-to-filter-alist
  '((flx-score . -100)
    (fussy-flx-rs-score . -100)
    (fussy-fuz-score . -100)
    (fussy-fuz-bin-score . -100)
    (fussy-fzf-native-score . 0)
    (fussy-hotfuzz-score . 0))
  "Candidates with scores of N or less are filtered for a given
`fussy-score-fn'.

Some backends such as `fussy-fuz-score' return negative scores
for low-quality matches.

Setting `fussy-score-threshold-to-filter' to a number will
override this alist.

If `fussy-score-fn' is not in the mapping, default to a threshold
of 0 wherever alist is used."
  :group 'fussy
  :type 'alist)

(defcustom fussy-max-word-length-to-score 400
  "Words that are longer than this length are not scored."
  :group 'fussy
  :type 'integer)

(defcustom fussy-propertize-fn
  #'fussy-propertize-common-part
  "Function used to propertize matches.

Takes STR \(to be propertized\) and
SCORE \(list of indices of STR to be propertized\).

This function is expected to return STR.

If this is nil, don't propertize (e.g. highlight matches) at all.
This can also be set to nil to assume highlighting from a different source.

e.g. `fussy-filter-orderless' can also be used for highlighting matches."
  :type `(choice
          (const :tag "No highlighting" nil)
          (const :tag "By completions-common face."
                 ,#'fussy-propertize-common-part)
          (const :tag "By flx propertization." ,'flx-propertize)
          (function :tag "Custom function"))
  :group 'fussy)

(defcustom fussy-compare-same-score-fn
  #'fussy-histlen->strlen<
  "Function used to compare matches with the same \\='completion-score.

FN takes in and compares two candidate strings C1 and C2 and
returns which candidates should have precedence.

If this is nil, do nothing."
  :type `(choice
          (const :tag "Don't compare candidates with same score." nil)
          (const :tag "Shorter candidates have precedence."
                 ,#'fussy-strlen<)
          (const :tag "Longer candidates have precedence."
                 ,#'fussy-strlen>)
          (const :tag "Recent candidates have precedence."
                 ,#'fussy-histlen<)
          (const :tag "Recent (then shorter length) candidates have precedence."
                 ,#'fussy-histlen->strlen<)
          (function :tag "Custom function"))
  :group 'fussy)

(defcustom fussy-max-limit-preferred-candidate-fn nil
  "Function used when collection length is greater than\

`fussy-max-candidate-limit'.

FN takes in and compares two candidate strings C1 and C2 and
returns which candidates should have precedence.

If this is nil, take the first `fussy-max-candidate-limit' number
of candidates that was returned by the completion table."
  :type `(choice
          (const :tag "Take the first X number of candidates." nil)
          (const :tag "Shorter candidates have precedence."
                 ,#'fussy-strlen<)
          (const :tag "Longer candidates have precedence."
                 ,#'fussy-strlen>)
          (const :tag "Recent candidates have precedence."
                 ,#'fussy-histlen<)
          (const :tag "Recent (then shorter length) candidates have precedence."
                 ,#'fussy-histlen->strlen<)
          (function :tag "Custom function"))
  :group 'fussy)

(defcustom fussy-filter-fn
  #'fussy-filter-flex
  "Function used for filtering candidates before scoring.

FN takes in the same arguments as `fussy-try-completions'.

This FN should not be nil.

Use either `fussy-filter-orderless' or `fussy-filter-default' for faster
filtering through the `all-completions' (written in C) interface.

If using `fussy-filter-default', `fussy-default-regex-fn' can be configured."
  :type `(choice
          (const :tag "Built in Flex Filtering"
                 ,#'fussy-filter-flex)
          (const :tag "Built in Faster Flex Filtering in C"
                 ,#'fussy-filter-default)
          (const :tag "Orderless Flex Filtering"
                 ,#'fussy-filter-orderless-flex)
          (const :tag "Orderless"
                 ,#'fussy-filter-orderless)
          (const :tag "By Scoring"
                 ,#'fussy-filter-by-scoring)
          (function :tag "Custom function"))
  :group 'fussy)

(defcustom fussy-default-regex-fn
  #'fussy-pattern-default
  "Function used to create regex for `fussy-filter-default'.

It takes in a STR and returns a regex usable with `all-completions'.

The return value of this FN is meant to be pushed to `completion-regexp-list'.

Flex 1 is what is used in `company-flx'.  It seems to be the fastest from an eye
test but all the regex are comparable in performance.

Flex 2 functions match the regex returned by `orderless-flex'.  Flex 2 functions
are more exhaustive than Flex 1 functions."
  :type `(choice
          (const :tag "Flex 1"
                 ,#'fussy-pattern-flex-1)
          (const :tag "Flex 2"
                 ,#'fussy-pattern-flex-2)
          (const :tag "Default"
                 ,#'fussy-pattern-default)
          (const :tag "First Letter"
                 ,#'fussy-pattern-first-letter)
          (function :tag "Custom function"))
  :group 'fussy)

(defcustom fussy-score-fn
  'flx-score
  "Function used for scoring candidates.

FN should at least take in STR and QUERY.

This may or may not be used by `fussy-score-ALL-fn'."
  :type `(choice
          (const :tag "Score using Flx"
                 ,'flx-score)
          (const :tag "Score using Flx-RS"
                 ,#'fussy-flx-rs-score)
          (const :tag "Score using FZF"
                 ,'fussy-fzf-native-score)
          (const :tag "Score using Fuz"
                 ,#'fussy-fuz-score)
          (const :tag "Score using Fuz-Bin"
                 ,#'fussy-fuz-bin-score)
          (const :tag "Score using LiquidMetal"
                 ,#'fussy-liquidmetal-score)
          (const :tag "Score using Sublime-Fuzzy"
                 ,#'fussy-sublime-fuzzy-score)
          (const :tag "Score using Hotfuzz"
                 ,#'fussy-hotfuzz-score)
          (function :tag "Custom function"))
  :group 'fussy)

(defcustom fussy-whitespace-ok-fns '(fussy-fzf-native-score)
  "List of `fussy-score-fn's that can accept whitespace."
  :type '(list function)
  :group 'fussy)

(defcustom fussy-AND-component-separator nil
  "Separator used to split the query string into AND components.
This mirrors `orderless-component-separator' and can be either:
- A regexp string passed to `split-string', e.g. \"[ &]\" to split on
  spaces or ampersands.
- A function that takes a string and returns a list of component strings,
  e.g. `orderless-escapable-split-on-space'.
When non-nil, the query is split on this separator and the resulting
components are rejoined with spaces (AND semantics) before being passed
to the scoring backend.  This is useful for in-buffer completion
(e.g. `company-mode') where pressing SPC would otherwise dismiss the
completion UI, allowing you to use a different separator character
(e.g. \"&\") for multi-token AND queries.
See also `fussy-OR-component-separator' for OR semantics.
When nil (the default), the query is passed to the scoring backend as-is.
Example:
  ;; Use & as an AND separator in addition to space:
  (setq fussy-AND-component-separator \"[ &]+\")
  ;; Or with a custom function matching orderless behavior:
  (setq fussy-AND-component-separator #\='orderless-escapable-split-on-space)"
  :type `(choice
          (const :tag "No separator (disabled)" nil)
          (string :tag "Regexp separator, e.g. \\"[ &]+\\"")
          (const :tag "Spaces" " +")
          (const :tag "Spaces, hyphen or slash" " +\\|[-/]")
          (function :tag "Custom function"))
  :group 'fussy)

(defcustom fussy-OR-component-separator nil
  "Separator used to split the query string into OR groups.
Can be either:
- A regexp string passed to `split-string', e.g. \"|\" or \"[ |]+\".
- A function that takes a string and returns a list of OR-group strings.
When non-nil, the query is first split on this separator into OR groups.
Each OR group is then processed by `fussy-AND-component-separator' (if set)
to produce AND components within the group.  OR groups are rejoined with
\" | \" which `fzf-native' understands natively as OR.
This maps to fzf's native | operator:
  e.g. \"d | x\" matches candidates containing either \"d\" OR \"x\".
       \"d x\" matches candidates containing both \"d\" AND \"x\".
Only meaningful when `fussy-score-fn' is `fussy-fzf-native-score' or
`fussy-score-ALL-fn' is `fussy-fzf-score', since other backends do not
understand the | OR operator.
Example — use | for OR and & for AND within each OR group:
  (setq fussy-OR-component-separator \"|\")    ;; split on |
  (setq fussy-AND-component-separator \"[ &]+\") ;; AND within each group
  ;; Typing \"abc&def|ghi\" scores as fzf query \"abc def | ghi\"
  ;; i.e. (abc AND def) OR ghi"
  :type `(choice
          (const :tag "No OR separator (disabled)" nil)
          (string :tag "Regexp OR separator, e.g. \\\"|\\\"")
          (function :tag "Custom function"))
  :group 'fussy)

(defcustom fussy-score-ALL-fn 'fussy-score
  "Function used for score ALL candidates.

FN should take in ARGS: candidates string &optional cache.

This function may call out to `fussy-score-fn' to score matches or
does the heavy lifting itself.

For example `fussy-score' makes use of `fussy-score-fn' but
`fussy-fzf-score' sends its entire collection to `fzf-native' instead."
  :type `(choice
          (const :tag "Default scoring"
                 ,'fussy-score)
          (const :tag "Scoring using `fzf-native-score-all'."
                 ,#'fussy-fzf-score)
          (function :tag "Custom function"))
  :group 'fussy)

(defcustom fussy-fuz-use-skim-p t
  "If t, use skim fuzzy matching algorithm with `fuz'.

If nil, use clangd fuzzy matching algorithm with `fuz'.

This boolean is only used if `fussy-fuz-score' is the `fussy-score-fn'."
  :group 'fussy
  :type 'boolean)

(defcustom fussy-score-fns-without-indices '(fussy-hotfuzz-score
                                             fussy-sublime-fuzzy-score
                                             fussy-liquidmetal-score)
  "List of scoring functions that only returns the score.

e.g. Instead of returning LIST SCORE MATCH_1 MATCH_2 which something like
`flx-score' does, it returns LIST SCORE.

Scoring functions in this list's highlighting are then taken care of by either

`fussy-filter-orderless' or `completion-pcm--hilit-commonality'.  See

`fussy--use-pcm-highlight-p'.

Functions in this list should match `fussy-score-fn'."
  :type '(list function)
  :group 'fussy)

(defcustom fussy-remove-bad-char-fn
  #'fussy-without-tofu-char
  "Function used to strip characters that some backends are unable to handle.

Some scoring backends \(e.g. Rust backends\) are unable to handle strings with
certain character encoding.  This function is applied to the candidate strings
before they are passed to the scoring function.

This was added specifically for `consult' but other encodings could also pose
a problem.  To keep the performance of the Rust backends useful,
`fussy-without-tofu-char' is set as the default function.
`fussy-without-tofu-char' is an order of magnitude faster than
`fussy-without-unencodeable-chars' but won't handle every case.

Another option is to use `fussy-encode-coding-string' which dumbly converts
a multibytestring without considering what the final string will look like.
Using this may work for the purpose of matching too as the final candidate
string may go from something like abcX to abcR where X was the multibyte char
that is not usable with the above scoring backends and R is a random ascii
character encoded from X.

This is set to nil if `fussy-setup' is called as we use the workaround
described here:
https://github.com/axelf4/hotfuzz?tab=readme-ov-file#dynamic-module
You can set this again if another encoding proves to be a problem.

For more information: \(https://github.com/minad/consult/issues/585\)"
  :type `(choice
          (const :tag "Remove Tofu"
                 ,#'fussy-without-tofu-char)
          (const :tag "Remove All"
                 ,#'fussy-without-unencodeable-chars)
          (const :tag "Convert to Unibyte"
                 ,#'fussy-encode-coding-string)
          (const :tag "Don't convert"
                 nil)
          (function :tag "Custom function"))
  :group 'fussy)

(defcustom fussy-prefer-prefix t
  "When using `fussy-filter-default', whether to prefer infix or prefix.

If t, prefix is used with `all-completions', if nil, use infix.

Infix is generally faster for `all-completions' but is not exhaustive.
Prefix can be slower but is exhaustive. For `completing-read',exhaustive
filtering is generally more preferable but for `completion-at-point-functions',
using infix can be a good tradeoff.

This variable should be let-bound/wrapped over `completion-at-point-functions',
e.g. `company-capf' and set to nil for typing performance and kept to t for
normal `completing-read' scenarios.

See comments in `fussy-filter-default' for examples of what infix or prefix
can look like."
  :type 'boolean
  :group 'fussy)

(defcustom fussy-filter-unscored-candidates t
  "Whether or not to filter unscored candidates.

This only applies when `fussy-max-candidate-limit' is reached."
  :type 'boolean
  :group 'fussy)

(defcustom fussy-use-cache nil
  "Whether or not to use cache in `fussy-all-completions'."
  :type 'boolean
  :group 'fussy)

(defcustom fussy-company-prefix-length 3
  "The prefix length before using `fussy' with `company'."
  :group 'fussy
  :type 'integer)

;;;###autoload
(defcustom fussy-adjust-metadata-fn
  #'fussy--adjust-metadata
  "Used for `completion--adjust-metadata' to adjust completion metadata.

`completion--adjust-metadata' is what is used to set up sorting of candidates
based on `completion-score'.  The default `flex' completion style in
`completion-styles' uses `completion--flex-adjust-metadata' which respects
the original completion table's sort functions:

  e.g. display-sort-function, cycle-sort-function

The default of `fussy-adjust-metadata-fn' is used instead to ignore
existing sort functions in favor of sorting based only on the scoring done by
`fussy-score-fn'."
  :type `(choice
          (const :tag "Adjust metadata using fussy."
                 ,#'fussy--adjust-metadata)
          (const :tag "Adjust metadata using flex."
                 ,#'completion--flex-adjust-metadata)
          (function :tag "Custom function"))
  :group 'fussy)

(defmacro fussy--measure-time (&rest body)
  "Measure the time it takes to evaluate BODY.
https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00087.html"
  `(let ((time (current-time)))
     (let ((result ,@body))
       (message "%.06f" (float-time (time-since time)))
       result)))

;;
;; (@* "defsubst" )
;;


(defsubst fussy-encode-coding-string (string)
  "Call `encode-coding-string' for STRING."
  (encode-coding-string string 'utf-8 t))

(defsubst fussy-without-bad-char (str)
  "Return STR without bad characters in them."
  (or (and fussy-remove-bad-char-fn
           (funcall fussy-remove-bad-char-fn str))
      str))

;;
;; (@* "Constants and Variables" )
;;

(defvar completion-lazy-hilit)
(defvar completion-lazy-hilit-fn)

(defvar-local fussy--hist-hash nil
  "Hash table representing `minibuffer-history-variable'.

KEYs are values in the list.
VALUES are positions of the values in the list.

See `fussy--history-hash-table'.")

(defvar-local fussy--score-threshold-to-filter-alist-cache nil
  "Cached value of threshold derived from alist for score functions.

If `fussy-score-threshold-to-filter' is non-nil, the cache is
ignored.

See `fussy-score-threshold-to-filter-alist'.")

(defvar-local fussy--all-cache nil
  "Hash table representing a cache for `fussy-all-completions'.")

(defvar-local fussy-can-adjust-metadata-p t
  "Variable to flip whether or not `fussy' can adjust metadata.

This is intended to be let-bound by users when they don't want any sorting.

See `fussy--adjust-metadata' for more details.")

;;
;; (@* "All Completions Interface/API" )
;;

;;;###autoload
(defun fussy-try-completions (string table pred point)
  "Try to flex-complete STRING in TABLE given PRED and POINT.

Implement `try-completions' interface by using `completion-flex-try-completion'."
  ;; (message "called `fussy-try-completions'...")
  (completion-flex-try-completion string table pred point))

(defvar-local fussy--current-result nil
  "Current result of `fussy-all-completions'.")
(defvar-local fussy--current-prefix nil
  "Current prefix of `fussy-all-completions'.")

;;;###autoload
(defun fussy-all-completions (string table pred point)
  "Wrap `fussy-all-completions-v1' with `while-no-input'.

If another input arrived -> t -> return current result.
If input was cancelled -> nil -> return nil.
If result came back -> :default -> return result."
  (pcase
      (while-no-input (fussy-all-completions-v1 string table pred point))
    ('nil
     ;; (message "fn: %S nil" 'fussy-all-completions)
     nil)
    ('t
     ;; (message "fn: %S quoteT" 'fussy-all-completions)
     (when (consp fussy--current-result)
       (nconc fussy--current-result (length fussy--current-prefix))))
    (`,collection
     ;; Collection can be 0 when there are no candidates returned.
     (when (consp collection)
       ;; (message (format "fn: %S collection: %s"
       ;;                  'fussy-all-completions collection))
       (when fussy-use-cache
         ;; (message "putting %s into hash with coll length %s"
         ;;          string (length collection))
         ;; (fussy--print-hash-table fussy--all-cache)
         (puthash string (cl-copy-list collection)
                  fussy--all-cache))
       (nconc collection (length fussy--current-prefix))))))

;;;###autoload
(defun fussy-all-completions-v1 (string table pred point)
  "Get flex-completions of STRING in TABLE, given PRED and POINT.

Implement `all-completions' interface with additional fuzzy / `flx' scoring."
  ;; (message "called `fussy-all-completions'...")
  (setf fussy--current-result nil)
  (setf fussy--hist-hash (fussy--history-hash-table))
  (when (and fussy-use-cache
             (or
              (not fussy--all-cache)
              (equal string "")))
    (setf fussy--all-cache
          (make-hash-table :test 'equal)))
  (when fussy-ignore-case
    ;; `completion-ignore-case' is usually set up in `minibuffer-with-setup-hook'.
    ;; e.g. `read-file-name-default'
    ;; Many search functions leverage this variable. In the case of fuzzy
    ;; matching, it is better to match insensitively.
    ;; For example, the implementation of `completion-pcm--hilit-commonality'
    ;; uses `case-fold-search' which sets its value to `completion-ignore-case'.
    ;; Other examples include `completion-pcm--all-completions' which is used by
    ;; `fussy-filter-flex'. `orderless-filter' and `all-completions' also use
    ;; this variable.
    (setq-local completion-ignore-case t))
  (let* ((metadata (completion-metadata string table pred))
         (cache (if (memq (completion-metadata-get metadata 'category)
                          '(file
                            project-file))
                    flx-file-cache
                  flx-strings-cache))
         (beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (prefix (substring beforepoint 0 (car bounds)))
         (infix (concat
                 (substring beforepoint (car bounds))
                 (substring afterpoint 0 (cdr bounds)))))
    (setf fussy--current-prefix prefix)
    (if-let ((cached-all (and fussy-use-cache
                              (cl-copy-list
                               (gethash string fussy--all-cache)))))
        (progn
          (setf fussy--current-result cached-all)
          ;; (message "%s from hash with length %d"
          ;;          string (length cached-all))
          ;; (fussy--print-hash-table fussy--all-cache)
          (fussy--highlight-collection
           (if (fussy--orderless-p)
               (fussy--recreate-orderless-pattern
                string table pred point)
             (fussy--recreate-regex-pattern
              beforepoint afterpoint bounds))
           cached-all))
      (pcase-let*
          ((`(,all ,pattern ,_prefix)
            (if-let ((cached-all
                      (and
                       fussy-use-cache
                       (length> string 0)
                       ;; e.g. ~/.emacs.d/url/ should not use entry from "~/.emacs.d/url".
                       (not (string-suffix-p "/" string))
                       (cl-copy-list
                        (gethash
                         (substring string 0 (- (length string) 1))
                         fussy--all-cache)))))
                (let* ((_ (setf fussy--current-result cached-all))
                       (pattern (if (fussy--orderless-p)
                                    (fussy--recreate-orderless-pattern
                                     string table pred point)
                                  (fussy--recreate-regex-pattern
                                   beforepoint afterpoint bounds)))
                       (candidates (if (fussy--filter-by-scoring-p)
                                       (fussy-outer-score cached-all infix cache)
                                     cached-all)))
                  ;; (message "using cache for filter")
                  (setf fussy--current-result candidates)
                  (list candidates pattern prefix))
              (funcall fussy-filter-fn
                       string table pred point))))
        ;; (message (format
        ;;           "fn: %S string: %s prefix: %s infix: %s all: %S pattern: %s"
        ;;           'fussy-all-completions
        ;;           string prefix infix (or all '("nada")) pattern))
        (when all
          (setf fussy--current-result all)
          (if (or (length> infix fussy-max-query-length)
                  (fussy--filter-by-scoring-p) ;; We don't need to score again.
                  (string= infix ""))
              (fussy--highlight-collection pattern all)
            (if (length< all fussy-max-candidate-limit)
                (fussy--highlight-collection
                 pattern
                 (fussy-outer-score all infix cache))
              (let ((unscored-candidates '())
                    (candidates-to-score '()))
                ;; Presort candidates by
                ;; `fussy-max-limit-preferred-candidate-fn'.
                (setf unscored-candidates
                      (if fussy-max-limit-preferred-candidate-fn
                          (sort
                           all fussy-max-limit-preferred-candidate-fn)
                        ;; If `fussy-max-limit-preferred-candidate-fn'
                        ;; is nil, we'll partition the candidates as is.
                        all))
                ;; Partition the candidates into sorted and unsorted groups.
                (dotimes (_n (* (length unscored-candidates)
                                fussy-percent-of-candidates-to-score))
                  (push (pop unscored-candidates) candidates-to-score)
                  (setf fussy--current-result candidates-to-score))
                (append
                 ;; Compute all of the fuzzy scores only for candidates.
                 (fussy--highlight-collection
                  pattern
                  (fussy-outer-score candidates-to-score infix cache))
                 unscored-candidates)))))))))

;;
;; (@* "Scoring & Highlighting" )
;;

(defun fussy-valid-score-p (score)
  "Return whether SCORE is valid."
  (and score
       ;; Score of '(nil) can be returned...
       (car score)
       (> (car score)
          (or fussy-score-threshold-to-filter
              fussy--score-threshold-to-filter-alist-cache
              (setq fussy--score-threshold-to-filter-alist-cache
                    (or (alist-get
                         fussy-score-fn
                         fussy-score-threshold-to-filter-alist)
                        0))))))

(defun fussy--normalize-and-components (string)
  "Split STRING into AND components using `fussy-AND-component-separator'.

Returns STRING with components joined by spaces (fzf AND semantics).
If `fussy-AND-component-separator' is nil, returns STRING unchanged."
  (if fussy-AND-component-separator
      (let ((components
             (if (functionp fussy-AND-component-separator)
                 (funcall fussy-AND-component-separator string)
               (split-string string fussy-AND-component-separator t))))
        (string-join components " "))
    string))

(defun fussy--normalize-or-components (string)
  "Split STRING into OR groups using `fussy-OR-component-separator'.

Returns STRING with OR groups joined by \" | \" (fzf OR semantics).
If `fussy-OR-component-separator' is nil, returns STRING unchanged."
  (if fussy-OR-component-separator
      (let ((or-groups
             (if (functionp fussy-OR-component-separator)
                 (funcall fussy-OR-component-separator string)
               (split-string string fussy-OR-component-separator t))))
        (string-join or-groups " | "))
    string))

(defun fussy-normalize-query (string)
  "Normalize STRING using component separators.

See `fussy-AND-component-separator' and `fussy-OR-component-separator'.

Applies `fussy--normalize-and-components' first, then
`fussy--normalize-or-components'.  AND normalization converts custom
separators (e.g. \"&\") into spaces within each token group.  OR
normalization then splits on the OR separator and rejoins groups with
\" | \" which `fzf-native' understands natively as the OR operator.

When both separators are nil, STRING is returned unchanged.

Examples (with fussy-OR-component-separator \"|\" and
          fussy-AND-component-separator \"[ &]+\"):
  \"foo|bar\"     -> \"foo | bar\"       (foo OR bar)
  \"foo&baz|bar\" -> \"foo baz | bar\"  (foo AND baz) OR bar
  \"foo bar\"     -> \"foo bar\"         (spaces already AND in fzf)"
  (fussy--normalize-or-components
   (fussy--normalize-and-components string)))

(defun fussy-outer-score (candidates string &optional cache)
  "Function used to wrap `fussy-score-ALL-fn'."
  (funcall fussy-score-ALL-fn candidates string cache))

(defun fussy-fzf-score (candidates string &optional _cache)
  "Score and propertize CANDIDATES using STRING.

This implementation uses `fzf-native-score-all' to do all its scoring in one go.

Ignore CACHE. This is only added to match `fussy-score'."
  (when (fboundp 'fzf-native-score-all)
    (let ((string (fussy-encode-coding-string
                   (fussy-normalize-query string))))
      (fzf-native-score-all candidates string))))

(defun fussy-score (candidates string &optional cache)
  "Score and propertize CANDIDATES using STRING.

Use CACHE for scoring.

Set a text-property \='completion-score on candidates with their score.
`completion--adjust-metadata' later uses this \='completion-score for sorting."
  (let ((result '())
        (string (fussy-encode-coding-string
                 (let ((normalized (fussy-normalize-query string)))
                   (if (memq fussy-score-fn fussy-whitespace-ok-fns)
                       normalized
                     (replace-regexp-in-string "\\\s" "" normalized))))))
    (dolist (x candidates)
      (setf x (copy-sequence x))
      (if (> (length x) fussy-max-word-length-to-score)
          ;; Don't score x but don't filter it out either.
          (unless fussy-filter-unscored-candidates
            (push x result))
        (let ((score (funcall fussy-score-fn x string cache)))
          ;; (message
          ;;  (format "fn: %S candidate: %s query: %s score %S"
          ;;          'fussy-score x string score))
          ;; Candidates with a score of N or less are filtered.
          (when (fussy-valid-score-p score)
            (put-text-property 0 1 'completion-score (car score) x)

            ;; If we're using pcm highlight, we don't need to propertize the
            ;; string here. This is faster than the pcm highlight but doesn't
            ;; seem to work with `find-file'.
            (when (fussy--should-propertize-p)
              (setf
               x (funcall fussy-propertize-fn x score)))
            (push x result)))))
    ;; Returns nil if empty.
    result))

(defun fussy--should-propertize-p ()
  "Whether or not to call `fussy-propertize-fn'.

If `fussy--use-pcm-highlight-p' is t, highlighting will be handled in
`fussy--maybe-highlight'.

If `fussy--orderless-p' is t, `fussy-filter-orderless' will take care of
highlighting.

If `fussy-propertize-fn' is nil, no highlighting should take place."
  (and
   (not (fussy--use-pcm-highlight-p))
   (not (fussy--orderless-p))
   fussy-propertize-fn))

(defun fussy-orderless--highlight-collection (regexps completions ignore-case)
  "Highlight COMPLETIONS using REGEXPS respecting IGNORE-CASE.

This is extracted from `orderless-all-completions' to do highlighting.
`orderless' returns the filtered collection immediately which lets it do its
highlighting after filtering. Since we sort and score the collection afterwards,
we need to highlight the collection later.

E.g. In `orderless': filter -> highlight -> return collection
In `fussy', filter* -> score# -> sort# -> highlight* -> return collection.

The * is taken care of by `orderless' and the # is taken care of by `fussy'.

The names of the parameters REGEXPS and COMPLETIONS match `orderless' to make it
easy to compare with the original but they are 1:1 with
`fussy--highlight-collection''s PATTERN and COLLECTION parameters."
  (when (fboundp 'orderless--highlight)
    (if completion-lazy-hilit
        (setq completion-lazy-hilit-fn
              (apply-partially #'orderless--highlight regexps ignore-case))
      (cl-loop for str in-ref completions do
               (setf str (orderless--highlight
                          regexps ignore-case (substring str))))))
  completions)

(defun fussy--highlight-collection (pattern collection)
  "Highlight COLLECTION using PATTERN.

  Only highlight if `fussy--use-pcm-highlight-p' is t."
  (when collection
    (cond
     ((fussy--use-pcm-highlight-p)
      (fussy--pcm-highlight pattern collection))
     ((fussy--orderless-p)
      (fussy-orderless--highlight-collection
       pattern collection completion-ignore-case))
     (:default
      ;; Assume that the collection's highlighting is handled elsewhere.
      collection))))

(defun fussy--pcm-highlight (pattern collection)
  "Highlight with pcm-style for COLLECTION using PATTERN.

pcm-style refers to using `completion-pcm--hilit-commonality' for highlighting."
  (completion-pcm--hilit-commonality pattern collection))

(defun fussy-propertize-common-part (str score)
  "Return propertized copy of STR according to score.

If SCORE does not have indices to highlight, return STR unmodified."
  (if (or
       ;; Has only score but no indices or nil.
       (<= (length score) 1)
       ;; Indices are higher than the length of str indicating the indices are
       ;; incorrect. Skip highlighting to avoid breaking completion.
       ;; Take the last index to compare against str because all indices need
       ;; to be less than the length of str in order for highlighting to work.
       (>= (car (last score)) (length str)))
      str
    ;; Has a score and an index to highlight.
    (let ((block-started (cadr score))
          (last-char nil)
          ;; Originally we used `substring-no-properties' when setting str but
          ;; that strips text properties that other packages may set.
          ;; One example is `consult', which sprinkles text properties onto
          ;; the candidate. e.g. `consult--line-prefix' will check for
          ;; 'consult-location on str candidate.
          (str (if (consp str) (car str) str)))
      (dolist (char (cdr score))
        (when (and last-char
                   (not (= (1+ last-char) char)))
          (add-face-text-property block-started (1+ last-char)
                                  'completions-common-part nil str)
          (setf block-started char))
        (setf last-char char))
      (add-face-text-property block-started (1+ last-char)
                              'completions-common-part nil str)
      (when (and
             last-char
             (> (length str) (+ 2 last-char)))
        (add-face-text-property (1+ last-char) (+ 2 last-char)
                                'completions-first-difference
                                nil
                                str))
      (if (consp str)
          (cons str (cdr str))
        str))))

;;
;; (@* "Bootstrap" )
;;

;;;###autoload
(progn
  (put 'fussy 'completion--adjust-metadata fussy-adjust-metadata-fn)
  (add-to-list 'completion-styles-alist
               '(fussy fussy-try-completions fussy-all-completions
                       "Smart Fuzzy completion with scoring.")))

;;;###autoload
(defun fussy-setup ()
  "Set up `fussy'."
  (unless (memq 'fussy completion-styles)
    (push 'fussy completion-styles))

  ;; https://github.com/minad/consult/issues/585
  ;; https://github.com/axelf4/hotfuzz?tab=readme-ov-file#dynamic-module
  (setq fussy-remove-bad-char-fn nil)
  (with-eval-after-load 'consult
    (defvar consult--tofu-char)
    (defvar consult--tofu-range)
    (setq consult--tofu-char #x100000
          consult--tofu-range #x00fffe))

  ;; For example, project-find-file uses 'project-files which uses
  ;; substring completion by default. Set our own defaults.
  (setq completion-category-overrides
        '((file ;; https://github.com/jojojames/fussy/issues/46
           (styles basic))
          (buffer
           (styles fussy basic))
          (consult-location (styles fussy basic))
          (unicode-name
           (styles fussy basic))
          (project-file
           (styles fussy))
          (xref-location
           (styles fussy))
          (info-menu
           (styles fussy basic))
          (symbol-help
           (styles fussy basic)))))

;;;###autoload
(defun fussy-setup-fzf ()
  "Set up `fussy' for `fzf-native'."
  (fussy-setup)
  (setq fussy-filter-fn 'fussy-filter-by-scoring)
  (setq fussy-score-fn 'fussy-fzf-native-score)
  (setq fussy-score-ALL-fn 'fussy-fzf-score)
  (setq fussy-use-cache t))

;;
;; (@* "Sorting" )
;;

(defun fussy--adjust-metadata (metadata)
  "If actually doing filtering, adjust METADATA's sorting."
  (let ((flex-is-filtering-p
         ;; JT@2019-12-23: FIXME: this is kinda wrong.  What we need
         ;; to test here is "some input that actually leads/led to
         ;; flex filtering", not "something after the minibuffer
         ;; prompt".  E.g. The latter is always true for file
         ;; searches, meaning we'll be doing extra work when we
         ;; needn't.
         (and
          fussy-can-adjust-metadata-p
          (or (not (window-minibuffer-p))
              (> (point-max) (minibuffer-prompt-end))))))
    `(metadata
      ,@(and flex-is-filtering-p
             `((display-sort-function . fussy--sort)))
      ,@(and flex-is-filtering-p
             `((cycle-sort-function . fussy--sort)))
      ,@(cdr metadata))))

(defun fussy--sort (completions)
  "Sort COMPLETIONS using `completion-score' and completion length."
  (sort
   completions
   (lambda (c1 c2)
     (let ((s1 (or (get-text-property 0 'completion-score c1) 0))
           (s2 (or (get-text-property 0 'completion-score c2) 0)))
       ;; (message (format "c1: %s score: %d" c1 s1))
       ;; (message (format "c2: %s score: %d" c2 s2))
       (if (and (= s1 s2)
                fussy-compare-same-score-fn)
           (funcall fussy-compare-same-score-fn c1 c2)
         ;; Candidates with higher completion score have precedence.
         (> s1 s2))))))

;;
;; (@* "Candidate Comparisons" )
;;

(defun fussy-strlen< (c1 c2)
  "Return t if C1's length is less than C2's length."
  (< (length c1) (length c2)))

(defun fussy-strlen> (c1 c2)
  "Return t if C1's length is greater than C2's length."
  (> (length c1) (length c2)))

(defun fussy-histlen< (c1 c2)
  "Return t if C1 occurred more recently than C2.

Check C1 and C2 in `minibuffer-history-variable' which is stored in
`fussy--hist-hash'."
  (if-let* ((hist fussy--hist-hash)
            (c1-pos (or (gethash c1 hist) most-positive-fixnum))
            (c2-pos (or (gethash c2 hist) most-positive-fixnum)))
      (< c1-pos c2-pos)
    nil))

(defun fussy-histlen->strlen< (c1 c2)
  "Return t if C1 occurs more recently than C2 or is shorter than C2."
  (if-let* ((hist fussy--hist-hash)
            (c1-pos (or (gethash c1 hist) most-positive-fixnum))
            (c2-pos (or (gethash c2 hist) most-positive-fixnum)))
      (if (= c1-pos c2-pos)
          (fussy-strlen< c1 c2)
        (< c1-pos c2-pos))
    (fussy-strlen< c1 c2)))

;;
;; (@* "Utils" )
;;

(defun fussy--recreate-orderless-pattern (string table pred _point)
  "See `fussy--recreate-regex-pattern'."
  ;; This implementation from `orderless-all-completions'.
  (if (fboundp 'orderless--compile)
      (pcase-let
          ((`(,_prefix ,regexps ,_ignore-case ,_pred)
            (if (eq fussy-filter-fn 'fussy-filter-orderless-flex)
                (let ((orderless-matching-styles '(orderless-flex)))
                  (ignore orderless-matching-styles)
                  (orderless--compile string table pred))
              (orderless--compile string table pred))))
        regexps)
    nil))

(defun fussy--recreate-regex-pattern (beforepoint afterpoint bounds)
  "Utility function to create regex pattern for highlighting.

`fussy--highlight-collection' consumes this pattern.
This usually comes out as a result of the initial filtering of candidates,
but when we're pulling from the cache, the pattern is not there, so we
rebuild it here. We could also try caching the pattern instead of creating it
again."
  (cond
   ((eq fussy-filter-fn 'fussy-filter-flex)
    ;; This comes from `fussy-emacs-legacy-completion-substring--all-completions'
    ;; Look at `fussy-filter-flex'.
    (let* ((basic-pattern (completion-basic--pattern
                           beforepoint afterpoint bounds))
           (pattern (if (not (stringp (car basic-pattern)))
                        basic-pattern
                      (cons 'prefix basic-pattern)))
           (pattern
            (completion-pcm--optimize-pattern
             (fussy-emacs-legacy-completion-flex--make-flex-pattern pattern))))
      pattern))
   ((and (eq fussy-filter-fn 'fussy-filter-by-scoring)
         (fussy--fzf-p))
    (fussy-make-fzf-highlight-pattern
     (concat (substring beforepoint (car bounds))
             (substring afterpoint 0 (cdr bounds)))))
   (:default ;; `fussy-filter-default'
    (fussy-make-pcm-highlight-pattern
     beforepoint afterpoint bounds))))

(defun fussy--fzf-p ()
  "Return whether or not we're using fzf."
  (or (eq fussy-score-ALL-fn 'fussy-fzf-score)
      (eq fussy-score-fn 'fussy-fzf-native-score)))

(defun fussy--filter-by-scoring-p ()
  "Return whether or not we're filtering matches through our scoring function."
  (eq fussy-filter-fn 'fussy-filter-by-scoring))

(defun fussy--orderless-p ()
  "Return whether or not we're using `orderless' for filtering."
  (or (eq fussy-filter-fn 'fussy-filter-orderless)
      (eq fussy-filter-fn 'fussy-filter-orderless-flex)))

(defun fussy--use-pcm-highlight-p ()
  "Check if highlighting should use `completion-pcm--hilit-commonality'.

Check if `fussy-score-fn' used doesn't return match indices.
Check if `orderless' is being used."
  (cond
   ;; If we're using `orderless' to filter, don't use pcm highlights because
   ;; `orderless' does it on its own.
   ((fussy--orderless-p) nil)
   ((fussy--filter-by-scoring-p) t)
   ;; `fussy-fzf-score' doesn't highlight on its own.
   ((eq fussy-score-ALL-fn 'fussy-fzf-score) t)
   ;; These don't generate match indices to highlight at all so we should
   ;; highlight with `completion-pcm--hilit-commonality'.
   ((memq fussy-score-fn fussy-score-fns-without-indices) t)
   (:default nil)))

(defun fussy--history-hash-table ()
  "Return hash table representing `minibuffer-history-variable'.

Key is the history string and Value is the history position."
  (when-let* ((hist (and (not (eq minibuffer-history-variable t))
                         (symbol-value minibuffer-history-variable)))
              (table (make-hash-table :test 'equal
                                      :size (length hist))))
    (cl-loop for index from 0
             for item in hist
             unless (gethash item table)
             do (puthash item index table))
    table))

(defun fussy-without-unencodeable-chars (string)
  "Strip invalid chars from STRING.

See `fussy-remove-bad-char-fn'."
  ;; https://emacs.stackexchange.com/questions/5732/how-to-strip-invalid-utf-8-characters-from-a-string
  (string-join
   (delq nil (mapcar (lambda (ch)
                       (encode-coding-char ch 'utf-8 'unicode))
                     string))))

(defconst fussy--consult--tofu-char #x200000
  "Special character used to encode line prefixes for disambiguation.
We use invalid characters outside the Unicode range.")

(defconst fussy--consult--tofu-range #x100000
  "Special character range.")

(defsubst fussy--consult--tofu-p (char)
  "Return non-nil if CHAR is a tofu."
  (<= fussy--consult--tofu-char char
      (+ fussy--consult--tofu-char fussy--consult--tofu-range -1)))

(defun fussy-without-tofu-char (string)
  "Strip unencodeable char from STRING.

See `fussy-remove-bad-char-fn'."
  (if (fussy--consult--tofu-p (aref string (- (length string) 1)))
      (substring string 0 (- (length string) 1))
    string))

(defun fussy--print-hash-table (table)
  "Print TABLE."
  (message "------------------------------------------------------------------")
  (maphash (lambda (key value)
             (message "key: %s # of elements: %s" key (length value)))
           table)
  (message "------------------------------------------------------------------"))

(defun fussy-wipe-cache (&rest _)
  "Wipe buffer local `fussy--all-cache'."
  ;; (message "Setting `fussy--all-cache' to nil..")
  (setf fussy--all-cache nil))

;;
;; (@* "Filtering" )
;;

(defun fussy-filter-orderless-flex (string table pred point)
  "Match STRING to the entries in TABLE.

Use `orderless' for filtering by passing STRING, TABLE and PRED to

`orderless-filter'.  _POINT is not used. This version sets up `orderless'
to only use the `orderless-flex' pattern."
  (require 'orderless)
  (let ((orderless-matching-styles '(orderless-flex)))
    (fussy-filter-orderless string table pred point)))

(defun fussy-filter-orderless (string table pred _point)
  "Match STRING to the entries in TABLE.

Use `orderless' for filtering by passing STRING, TABLE and PRED to

`orderless-filter'.  _POINT is not used."
  (require 'orderless)
  (when (and (fboundp 'orderless--filter)
             (fboundp 'orderless--compile))
    (pcase-let ((`(,prefix ,regexps ,ignore-case ,pred)
                 (orderless--compile string table pred)))
      (when-let ((completions (orderless--filter
                               prefix regexps ignore-case table pred)))
        (list completions regexps prefix)))))

(defun fussy-filter-flex (string table pred point)
  "Match STRING to the entries in TABLE.

Respect PRED and POINT.  The filter here is the same as in
`completion-flex-all-completions'."
  (pcase-let ((`(,completions ,pattern ,prefix ,_suffix ,_carbounds)
               (fussy-emacs-legacy-completion-substring--all-completions
                string
                table pred point
                #'fussy-emacs-legacy-completion-flex--make-flex-pattern)))
    (list completions pattern prefix)))

(defun fussy-filter-default (string table pred point)
  "Match STRING to the entries in TABLE.

Respect PRED and POINT.  This filter uses the `all-completions' interface
that's written in C for faster filtering."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (prefix (substring beforepoint 0 (car bounds)))
         (infix (concat
                 (substring beforepoint (car bounds))
                 (substring afterpoint 0 (cdr bounds))))
         (regexp (funcall fussy-default-regex-fn infix))
         (completion-regexp-list regexp)
         ;; Commentary on why we prefer prefix over infix.
         ;; For `find-file', if the prefix exists, we're in a different
         ;; directory, so should be retrieving candidates from that directory
         ;; instead.
         ;; ex. We started in ~/ home directory. User starts typing cod.
         ;; infix will be: c -> co -> cod
         ;; prefix will be ~/
         ;; User then enters a directory called ~/Code and types abc.
         ;; infix will be: a -> ab -> abc
         ;; prefix will be ~/Code
         ;; For `project-find-file', the prefix will usually be empty and only
         ;; the infix will be matched against.
         ;; So, *knock on wood*, it seems safe to prefer prefix completion over
         ;; infix completion.
         (completions
          ;; Is there an easier way to check if string is empty or nil?
          (if (or (/= (length prefix) 0)
                  fussy-prefer-prefix)
              ;; Always use prefix if available for correctness.
              ;; For example, `find-file', should always use prefix.
              (or (all-completions prefix table pred)
                  (all-completions infix table pred))
            ;; When prefix is nil, the choice if infix or prefix is preference..
            ;; Infix is much faster than prefix but can be "wrong" or not
            ;; exhaustive for matches. Prefix will be exhaustive and "correct"
            ;; but can be slow. Generally, we should prefer prefix for
            ;; correctness.
            ;; We allow an escape hatch to infix for extra performance with
            ;; `fussy-prefer-prefix' set to nil.
            (or (all-completions infix table pred)
                (all-completions prefix table pred))))
         ;; Create this pattern for the sole purpose of highlighting with
         ;; `completion-pcm--hilit-commonality'. We don't actually need this
         ;; for `all-completions' to work since we're just using
         ;; `completion-regexp-list' with `all-completions'.
         ;; In addition to that, we only need this pattern if we're highlighting
         ;; using `completion-pcm--hilit-commonality' so skip evaluating the
         ;; pattern if this is not the pcm highlight case.
         (pattern
          (fussy-make-pcm-highlight-pattern beforepoint afterpoint bounds)))
    ;; (message
    ;;  (format
    ;;   "prefix: %s infix: %s pattern %s completions %S regexp_list: %S"
    ;;   prefix infix pattern completions completion-regexp-list))
    (list completions pattern prefix)))

(defun fussy-filter-by-scoring (string table pred point)
  "Match STRING to the entries in TABLE.

Use `fussy-score-ALL-fn' for filtering."
  (let*
      ((beforepoint (substring string 0 point))
       (afterpoint (substring string point))
       (bounds (completion-boundaries beforepoint table pred afterpoint))
       (prefix (substring beforepoint 0 (car bounds)))
       (infix (concat
               (substring beforepoint (car bounds))
               (substring afterpoint 0 (cdr bounds))))
       (pred-infix
        (apply-partially 'fussy-filter-by-scoring-predicate infix table pred))
       (completion-regexp-list nil)
       ;; (_ (message (format "prefix: %s infix: %s" prefix infix)))
       (completions
        (all-completions prefix table pred-infix))
       (pattern
        (fussy--recreate-regex-pattern beforepoint afterpoint bounds)))
    (list completions pattern prefix)))

(defun fussy-filter-by-scoring-predicate (string table pred candidate
                                                 &optional hash-table-value)
  ;; From `all-completions' documentation.
  ;; COLLECTION can also be a function to do the completion itself.
  ;; It receives three arguments: STRING, PREDICATE and t.
  ;; Whatever it returns becomes the value of `all-completions'.
  ;; If optional third argument PREDICATE is non-nil, it must be a function
  ;; of one or two arguments, and is used to test each possible completion.
  ;; A possible completion is accepted only if PREDICATE returns non-nil.
  ;;
  ;; The argument given to PREDICATE is either a string or a cons cell (whose
  ;; car is a string) from the alist, or a symbol from the obarray.
  ;; If COLLECTION is a hash-table, PREDICATE is called with two arguments:
  ;; the string key and the associated value.
  "Predicate for `all-completions' api.

STRING should be applied partially.
TABLE should be applied partially.
PRED should be applied partially.
See `fussy-filter-by-scoring'.

CANDIDATE is the possible completion, either a string, a cons cell
(whos car is a string), or a symbol from the obarray.

If COLLECTION is a hash-table, candidate is the string key and
hash-table-value is the associated value."
  ;; First call pred, if pred returns t, we can proceed to filter with the next step.
  ;; If pred returns nil, we don't need to filter
  (if (and pred
           (not (if (hash-table-p table)
                    (funcall pred candidate hash-table-value)
                  (funcall pred candidate))))
      nil
    ;; e.g. (> (car (fzf-native-score "abc" "a" )) 0)
    (if-let* ((x (cond
                  ((hash-table-p table) candidate)
                  ((stringp candidate) candidate)
                  ((consp candidate) (car candidate))
                  ((symbolp candidate) (symbol-name candidate))
                  (:default nil))))
        ;; (message (format "c: %s s: %s score: %d" x string (car score)))
        (fussy-valid-score-p (funcall fussy-score-fn x (fussy-normalize-query string)))
      t)))

(defun fussy-make-fzf-highlight-pattern (infix)
  "Create a pcm pattern based on fzf rules for highlighting.
INFIX is the fzf query string."
  (let ((tokens (split-string infix " " t))
        (pattern (list 'prefix)))
    (dolist (token tokens)
      (cond
       ;; inverse
       ((string-prefix-p "!" token) nil)
       ;; exact
       ((string-prefix-p "'" token)
        (push 'any pattern)
        (push (substring token 1) pattern))
       ;; prefix-exact
       ((string-prefix-p "^" token)
        (push 'any pattern)
        (push (substring token 1) pattern))
       ;; suffix-exact
       ((string-suffix-p "$" token)
        (push 'any pattern)
        (push (substring token 0 -1) pattern))
       ;; fuzzy
       (t
        (push 'any pattern)
        (dolist (char (append token nil))
          (push (string char) pattern)
          (push 'any pattern))
        (pop pattern)))) ;; remove last 'any
    (completion-pcm--optimize-pattern (nreverse pattern))))

(defun fussy-make-pcm-highlight-pattern (beforepoint afterpoint bounds)
  "Create flex pattern for highlighting.

Respect BEFOREPOINT, AFTERPOINT, and BOUNDS."
  (when (fussy--use-pcm-highlight-p)
    ;; Note to self:
    ;; The way we create the pattern here can be found in
    ;; `fussy-emacs-legacy-completion-substring--all-completions'.
    (let* ((basic-pattern (completion-basic--pattern
                           beforepoint afterpoint bounds))
           (pattern (if (not (stringp (car basic-pattern)))
                        basic-pattern
                      (cons 'prefix basic-pattern))))
      (completion-pcm--optimize-pattern
       (fussy-emacs-legacy-completion-flex--make-flex-pattern pattern)))))

;;
;; (@* "Pattern Compiler" )
;;
;; Random note:
;; These return something similar to what `orderless-pattern-compiler'
;; These can be applied where `orderless-pattern-compiler' can apply.
;; e.g. They return \(list some-regex\).
;;

(defun fussy-pattern-flex-1 (str)
  "Make STR flex pattern.

This may be the fastest regex to use but is not exhaustive."
  (list
   (concat "\\`"
           (mapconcat
            (lambda (x)
              (setf x (string x))
              (concat "[^" x "]*" (regexp-quote x)))
            str
            ""))))

(defun fussy-pattern-flex-2 (str)
  "Make STR flex pattern.

This is a copy of the `orderless-flex' pattern written without `rx'.

This one may be slower than `fussy-pattern-flex-1' but is more
exhaustive on matches."
  (list
   (concat
    (when (> (length str) 1)
      "\\(?:\\(?:")
    (mapconcat
     (lambda (x)
       (format "\\(%c\\)" x))
     str
     ".*")
    (when (> (length str) 1)
      "\\)\\)"))))

(defun fussy-pattern-default (str)
  "Make STR flex pattern.

If length if STR is somewhat long, return nil instead as long flex patterns
can be really slow when filtering."
  (if (> (length str) 4)
      nil
    (fussy-pattern-flex-2 str)))

(defun fussy-pattern-first-letter (str)
  "Make pattern for STR.

str: abc
result: LIST ^a"
  (if (and str (> (length str) 0))
      `(,(format "^%s" (substring str 0 1)))
    nil))

;;
;; (@* "Integration with other Packages" )
;;

;; `eglot' integration
;;;###autoload
(defun fussy-eglot-setup ()
  "Set up `fussy' with `eglot'."
  (with-eval-after-load 'eglot
    ;; `eglot' defaults to flex, so set an override to point `fussy' instead.
    (add-to-list 'completion-category-overrides
                 '(eglot-capf (styles fussy eglot--dumb-flex)))
    (add-to-list 'completion-category-overrides
                 '(eglot (styles fussy basic)))))

;; `company' integration.
(defvar company-backend)
(defvar company-prefix)

(defun fussy-company-sort-by-completion-score (candidates)
  "`company' transformer to sort CANDIDATES."
  (if (functionp company-backend)
      candidates
    (fussy--sort candidates)))

(defun fussy-company--transformer (f &rest args)
  "Advise `company--transform-candidates'."
  (if (length< company-prefix fussy-company-prefix-length)
      ;; Transform normally for short prefixes.
      (let ((fussy-can-adjust-metadata-p nil))
        (apply f args))
    (let ((company-transformers
           ;; `fussy-score' still needs to do sorting.
           ;; `fussy-fzf-score' sorts on its own.
           (if (eq fussy-score-ALL-fn 'fussy-score)
               '(fussy-company-sort-by-completion-score)
             '())))
      ;; Warning: Unused lexical variable `company-transformers'
      (ignore company-transformers)
      (apply f args))))

(defun fussy-company--fetch-candidates (f &rest args)
  "Advise `company--fetch-candidates'."
  (let ((prefix (nth 0 args))
        (_suffix (nth 1 args)))
    (if (length< prefix fussy-company-prefix-length)
        (let ((completion-styles (remq 'fussy completion-styles))
              (completion-category-overrides nil)
              (fussy-can-adjust-metadata-p nil))
          (apply f args))
      (let ((fussy-max-candidate-limit 5000)
            (fussy-default-regex-fn 'fussy-pattern-first-letter)
            (fussy-prefer-prefix nil))
        (apply f args)))))

(defun fussy-company--preprocess-candidates (candidates)
  "Advise `company--preprocess-candidates'.

This is to try to avoid a additional sort step."
  ;; (cl-assert (cl-every #'stringp candidates))
  ;; (unless (company-call-backend 'sorted)
  ;;   (setq candidates (sort candidates 'string<)))
  (when (and (fboundp 'company-call-backend)
             (fboundp 'company--strip-duplicates))
    (when (company-call-backend 'duplicates)
      (company--strip-duplicates candidates)))
  candidates)

(defun fussy-company-setup ()
  "Set up `company' with `fussy'."
  (with-eval-after-load 'company
    (advice-add 'company-auto-begin :before 'fussy-wipe-cache)
    (advice-add 'company--transform-candidates
                :around 'fussy-company--transformer)
    (advice-add 'company--fetch-candidates
                :around 'fussy-company--fetch-candidates)
    (advice-add 'company--preprocess-candidates
                :override 'fussy-company--preprocess-candidates)))

;; `fuz' integration.
(declare-function "fuz-fuzzy-match-skim" "fuz")
(declare-function "fuz-calc-score-skim" "fuz")
(declare-function "fuz-fuzzy-match-clangd" "fuz")
(declare-function "fuz-calc-score-clangd" "fuz")

(defun fussy-flx-rs-score (str query &rest args)
  "Score STR for QUERY with ARGS using `flx-rs-score'."
  (require 'flx-rs)
  (when (fboundp 'flx-rs-score)
    (flx-rs-score (fussy-without-bad-char str) query args)))

(defun fussy-fuz-score (str query &rest _args)
  "Score STR for QUERY using `fuz'.

skim or clangd algorithm can be used.

If `orderless' is used for filtering, we skip calculating matches
for more speed."
  (require 'fuz)
  (let ((str (fussy-without-bad-char str)))
    (if fussy-fuz-use-skim-p
        (if (fussy--orderless-p)
            (when (fboundp 'fuz-calc-score-skim)
              (list (fuz-calc-score-skim query str)))
          (when (fboundp 'fuz-fuzzy-match-skim)
            (fuz-fuzzy-match-skim query str)))
      (if (fussy--orderless-p)
          (when (fboundp 'fuz-calc-score-clangd)
            (list (fuz-calc-score-clangd query str)))
        (when (fboundp 'fuz-fuzzy-match-clangd)
          (fuz-fuzzy-match-clangd query str))))))

;; `fuz-bin' integration.
(declare-function "fuz-bin-dyn-score-skim" "fuz-bin")
(declare-function "fuz-bin-score-skim" "fuz-bin")
(declare-function "fuz-bin-dyn-score-clangd" "fuz-bin")
(declare-function "fuz-bin-score-clangd" "fuz-bin")

(defun fussy-fuz-bin-score (str query &rest _args)
  "Score STR for QUERY using `fuz-bin'.

skim or clangd algorithm can be used.

If `orderless' is used for filtering, we skip calculating matches
for more speed."
  (require 'fuz-bin)
  ;; (message (format "before: str: %s query: %s" str query))
  (let ((str (fussy-without-bad-char str)))
    ;; (message (format "after: str: %s query: %s" str query))
    (if fussy-fuz-use-skim-p
        (if (fussy--orderless-p)
            (when (fboundp 'fuz-bin-dyn-score-skim)
              (list (fuz-bin-dyn-score-skim query str)))
          (when (fboundp 'fuz-bin-score-skim)
            (fuz-bin-score-skim query str)))
      (if (fussy--orderless-p)
          (when (fboundp 'fuz-bin-dyn-score-clangd)
            (list (fuz-bin-dyn-score-clangd query str)))
        (when (fboundp 'fuz-bin-score-clangd)
          (fuz-bin-score-clangd query str))))))

;; `liquidmetal' integration
(declare-function "liquidmetal-score" "liquidmetal")

(defun fussy-liquidmetal-score (str query &rest _args)
  "Score STR for QUERY using `liquidmetal'.

This should be paired with `fussy-filter-orderless' to obtain match
highlighting."
  (require 'liquidmetal)
  (when (fboundp 'liquidmetal-score)
    (list (liquidmetal-score (fussy-without-bad-char str) query))))

;; `sublime-fuzzy' integration
(declare-function "sublime-fuzzy-score" "sublime-fuzzy")

(defun fussy-sublime-fuzzy-score (str query &rest _args)
  "Score STR for QUERY using `sublime-fuzzy'."
  (require 'sublime-fuzzy)
  (when (fboundp 'sublime-fuzzy-score)
    (list (sublime-fuzzy-score query (fussy-without-bad-char str)))))

;; `fzf-native' integration
(defvar fussy--fzf-native-slab nil)
(defsubst fussy--fzf-native-slab ()
  "Return lazy loaded slab for `fzf-native'."
  (or fussy--fzf-native-slab
      (when (fboundp 'fzf-native-make-default-slab)
        (setf fussy--fzf-native-slab (fzf-native-make-default-slab)))))

(defun fussy-fzf-native-score (str query &rest _args)
  "Score STR for QUERY using `fzf-native'."
  (require 'fzf-native)
  (when (fboundp 'fzf-native-score)
    (fzf-native-score
     (fussy-without-bad-char str) query (fussy--fzf-native-slab))))

;; `hotfuzz' integration
(declare-function "hotfuzz--cost" "hotfuzz")

(defun fussy-hotfuzz-score (str query &rest _args)
  "Score STR for QUERY using `hotfuzz'."
  (require 'hotfuzz)
  (when (fboundp 'hotfuzz--cost)
    ;; Looks like the score is flipped for `hotfuzz'.
    ;; See `hotfuzz-all-completions'.
    (list (+ 10000 (- (hotfuzz--cost query str))))))

;;; Legacy

;; This package uses internal Emacs functions.  Some of these functions
;; were modified in https://github.com/emacsmirror/emacs/commit/aa181cd35220,

(defun fussy-emacs-legacy-completion-flex--make-flex-pattern (pattern)
  "Convert PCM-style PATTERN into PCM-style flex pattern.

This turns
    (prefix \"foo\" point)
into
    (prefix \"f\" any \"o\" any \"o\" any point)
which is at the core of flex logic.  The extra
`any' is optimized away later on."
  (mapcan (lambda (elem)
            (if (stringp elem)
                (mapcan (lambda (char)
                          (list (string char) 'any))
                        elem)
              (list elem)))
          pattern))

(defun fussy-emacs-legacy-completion-substring--all-completions
    (string table pred point &optional transform-pattern-fn)
  "Match the presumed substring STRING to the entries in TABLE.
Respect PRED and POINT.  The pattern used is a PCM-style
substring pattern, but it be massaged by TRANSFORM-PATTERN-FN, if
that is non-nil."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (suffix (substring afterpoint (cdr bounds)))
         (prefix (substring beforepoint 0 (car bounds)))
         (basic-pattern (completion-basic--pattern
                         beforepoint afterpoint bounds))
         (pattern (if (not (stringp (car basic-pattern)))
                      basic-pattern
                    (cons 'prefix basic-pattern)))
         (pattern (completion-pcm--optimize-pattern
                   (if transform-pattern-fn
                       (funcall transform-pattern-fn pattern)
                     pattern)))
         (all (completion-pcm--all-completions prefix pattern table pred)))
    (list all pattern prefix suffix (car bounds))))

(provide 'fussy)
;;; fussy.el ends here
