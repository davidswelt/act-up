;; Basic ACT-UP mode for Emacs 22 (and later)
;; (C) David Reitter, Carnegie Mellon University 2010
;; Released under the GNU Public License

;; Should work with GNU Emacs 22 (and later)
;; and Aquamacs 1.9 (and later)


;; To install, put it where Emacs can find it.
;; Typical install locations include ~/Library/Preferences/Aquamacs Emacs/
;; and /usr/local/share/emacs/site-lisp

;; Then, add this to your .emacs (or Preferences.el in Aquamacs):
;; (require 'act-up)

;; You can also directly specify a path to this file, e.g.:
;; (require 'act-up "/Users/dr/modeling/ACT-UP/doc/act-up.el")
 

;; CODE

;; extract these with "make symbols" in doc/  (ACT-UP distribution):
;; must change to lower case.
(setq ACT-UP-SYMS (mapcar (lambda (x) 
			      (format "%s" x))
			      '(*le* *bll* *ol* *procedure-compilation* pass-time best-chunk *epl* request-best-chunk meta-process-name module-busy-p debug-detail* stop-actup-time learn-chunk reset-module defproc debug-clear *associative-learning* *debug-to-log* *ms* debug-log *alpha* debug-detail set-dm-total-presentations reset-sji-fct *all* *iu* *egs* *ul* *dat* make-chunk* show-chunks set-base-levels-fct receive *mp* request blend set-base-level-fct pc *blc* *current-actup-meta-process* explain-activation with-current-model *maximum-associative-strength* model-chunks *ut* *lf* *au-rfr* current-model *detailed* *debug* wait-for-model debug-grep *warning* make-model show-utilities terminate-request request-filter-chunks retrieve-chunk response-available-p request-retrieve-chunk set-similarities-fct wait-for-response *rt* set-current-model blend-retrieve-chunk make-meta-process define-chunk-type wait-for-module *mas* *au-rpps* *critical* make-chunk reset-mp reset-model chunk-type *informational* meta-process flush-procedure-queue *nu* add-sji-fct assign-reward* *pas* assign-reward *ans* non-nil actup-chunk define-slots chunk-name actup-time filter-chunks *md* model-name)))


(define-derived-mode act-up-mode lisp-mode "ACT-UP"
  "Major mode for editing ACT-UP cognitive models.
Provides a binding for C-c C-d h to look up symbols
in the Common Lisp HyperSpec or in the ACT-UP documentation.
\\{act-up-mode-map}"
  (show-paren-mode 1)
  (mapcar (lambda (s)
	    (intern s common-lisp-hyperspec-symbols))
	  ACT-UP-SYMS)
  (setq slime-documentation-lookup-function #'act-up-slime-hyperspec-lookup))

(font-lock-add-keywords 'act-up-mode 
			(mapcar (lambda (s)
				  (cons (regexp-quote s)
					'font-lock-keyword-face))
				ACT-UP-SYMS))

(defun lookup-act-up-symbol (string)
  (interactive "MLook up symbol (ACT-UP): ")
  (browse-url (format "http://act-up.psy.cmu.edu/ACT-UP.html#%s" string)))

(defvar act-up-common-lisp-hyperspec-symbols nil)
(defun act-up-slime-hyperspec-lookup (symbol-name)
  "Look up a symbol in the Common Lisp HyperSpec or ACT-UP documentation.
A wrapper for `hyperspec-lookup'"
  (interactive (list (let* ((symbol-at-point (slime-symbol-at-point))
			    (completion-ignore-case t)
                            (stripped-symbol 
                             (and symbol-at-point
                                  (downcase
                                   (common-lisp-hyperspec-strip-cl-package 
                                    symbol-at-point)))))
                       (if (and stripped-symbol
				(or 
				 (member-ignore-case stripped-symbol ACT-UP-SYMS)
				 (intern-soft stripped-symbol
					      common-lisp-hyperspec-symbols)))
                           stripped-symbol
			 (completing-read
			  "Look up symbol (Common Lisp or ACT-UP): "
			  common-lisp-hyperspec-symbols nil ;; #'boundp
			  t stripped-symbol
			  'common-lisp-hyperspec-history)))))
  (if (member-ignore-case symbol-name ACT-UP-SYMS)
      (lookup-act-up-symbol symbol-name)
    (hyperspec-lookup symbol-name)))


(provide 'act-up)