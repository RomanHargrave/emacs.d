(setq custom-file
      (concat user-emacs-directory "init.el.d/ZZ-custom.el"))

(message "custom file: %s" custom-file)

(defalias 'tramp-compat-rx #'rx)

;; Hack, because bundled tramp is too new(?) for packages, but the
;; autoloader gets confused and there is just generally spookiness.
(when-let* ((tramp-el        (concat user-emacs-directory "straight/repos/tramp/tramp.el"))
	    (tramp-available (file-exists-p tramp-el)))
  (load-file tramp-el))

;; set up straight and pull in org from upstream rather than bundled
(defvar boostrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(defvar local-pkg
  (expand-file-name "dist-packages" user-emacs-directory))
(add-to-list 'load-path local-pkg)
(setq use-package-hook-name-suffix nil)

(straight-use-package 'f)
(require 'f)

(straight-use-package 'org)
(require 'org)

(defmacro rh/expand-all-paths (paths)
  "Collect expansion results for all paths and return them as a
flattened sequence"
  `(flatten-list (cl-mapcar 'file-expand-wildcards ,paths)))

(defun rh/load-tangled (file)
  "Load a tangled FILE, using a hidden temporary file"
  (let* ((dir          (f-dirname file))
	 (base         (f-base file))
	 (tangled-file (concat dir "/." (file-name-sans-extension base) ".el")))
    (when (file-newer-than-file-p file tangled-file)
      (org-babel-tangle-file file
			     tangled-file
			     (rx string-start (or "emacs-lisp" "elisp") string-end))
      (set-file-times tangled-file)
      (load-file tangled-file)
      (message "rh/load-tangled %s"))))

(defun rh/load-maybe-tangled (file)
  "Given FILE load the file, untangling as needed (if it ends with .org)"
  (when-let*
      ((visible (not (f-hidden-p file 'last)))
       (primary (not (backup-file-name-p file)))
       (extn    (file-name-extension file))
       (load-fn (cond ((string-equal extn "el")
                       (function load-file))
                      ((string-equal extn "org")
                       (function rh/load-tangled)))))
    (when (apply load-fn `(,file))
      file)))

(defun rh/load-el-dirs (&rest dir-names)
  "Load all understood files in each directory in DIR-NAMES"
  (cl-mapcar 'rh/load-maybe-tangled
             (rh/expand-all-paths 
              (cl-mapcar
               (lambda (dir-name)
                 (concat user-emacs-directory dir-name "/*.*"))
               dir-names))))

(rh/load-el-dirs "init.el.d")
