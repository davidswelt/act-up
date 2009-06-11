(defun update-emacs-R ()
  (interactive)
  (ess-force-buffer-current "Process to load into: ")
  (let ((sprocess ))
    (process-send-string (get-ess-process ess-current-process-name) "pl()\n"))
)

