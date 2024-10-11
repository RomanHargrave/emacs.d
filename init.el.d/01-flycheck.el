(use-package flycheck-languagetool
  :hook ((org-mode . rh/flycheck-select-languagetool)
         (latex-mode . rh/flycheck-select-languagetool))
  :config
  (defun rh/flycheck-select-languagetool ()
      (flycheck-select-checker 'languagetool))
  (defun flycheck-languagetool--start-server ()
    "Start the LanguageTool server (custom redefinition for app-text/languagetool)."
    (unless (process-live-p (get-process "languagetool-server"))
      (let ((process
	     (apply #'start-process
		    "languagetool-server"
		    "*LanguageTool server*"
		    "languagetool-server"
		    "--port" (format "%s" flycheck-languagetool-server-port)
		    flycheck-languagetool-server-args)))
	(set-process-query-on-exit-flag process nil)
	(while
	    (with-current-buffer (process-buffer process)
	      (goto-char (point-min))
	      (unless (re-search-forward " Server started$" nil t)
		(accept-process-output process 1)
		(process-live-p process)))))))
  (defun flycheck-languagetool--enabled ()
    "Return true, since there is no need to specify a Jar here."
    t))
