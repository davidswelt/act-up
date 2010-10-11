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

(defvar ACT-UP-SYMS '(*LE* *BLL* *OL* *PROCEDURE-COMPILATION* PASS-TIME BEST-CHUNK *EPL* REQUEST-BEST-CHUNK META-PROCESS-NAME MODULE-BUSY-P DEBUG-DETAIL* STOP-ACTUP-TIME LEARN-CHUNK RESET-MODULE DEFPROC DEBUG-CLEAR *ASSOCIATIVE-LEARNING* *DEBUG-TO-LOG* *MS* DEBUG-LOG *ALPHA* DEBUG-DETAIL SET-DM-TOTAL-PRESENTATIONS RESET-SJI-FCT *ALL* *IU* *EGS* *UL* *DAT* MAKE-CHUNK* SHOW-CHUNKS SET-BASE-LEVELS-FCT RECEIVE *MP* REQUEST BLEND SET-BASE-LEVEL-FCT PC *BLC* *CURRENT-ACTUP-META-PROCESS* EXPLAIN-ACTIVATION WITH-CURRENT-MODEL *MAXIMUM-ASSOCIATIVE-STRENGTH* MODEL-CHUNKS *UT* *LF* *AU-RFR* CURRENT-MODEL *DETAILED* *DEBUG* WAIT-FOR-MODEL DEBUG-GREP *WARNING* MAKE-MODEL SHOW-UTILITIES TERMINATE-REQUEST REQUEST-FILTER-CHUNKS RETRIEVE-CHUNK RESPONSE-AVAILABLE-P REQUEST-RETRIEVE-CHUNK SET-SIMILARITIES-FCT WAIT-FOR-RESPONSE *RT* SET-CURRENT-MODEL BLEND-RETRIEVE-CHUNK MAKE-META-PROCESS DEFINE-CHUNK-TYPE WAIT-FOR-MODULE *MAS* *AU-RPPS* *CRITICAL* MAKE-CHUNK RESET-MP RESET-MODEL CHUNK-TYPE *INFORMATIONAL* META-PROCESS FLUSH-PROCEDURE-QUEUE *NU* ADD-SJI-FCT ASSIGN-REWARD* *PAS* ASSIGN-REWARD *ANS* NON-NIL ACTUP-CHUNK DEFINE-SLOTS CHUNK-NAME ACTUP-TIME FILTER-CHUNKS *MD* MODEL-NAME)) 


(define-derived-mode act-up-mode lisp-mode "ACT-UP"
  "Major mode for editing ACT-UP cognitive models.
\\{act-up-mode-map}"
  (show-paren-mode 1))


(font-lock-add-keywords 'act-up-mode 
			(mapcar (lambda (s)
				  (cons (regexp-quote (format "%s" s))
					'font-lock-keyword-face))
				ACT-UP-SYMS))


(provide 'act-up)