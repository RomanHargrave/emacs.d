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
(defvar local-pkg (expand-file-name "dist-packages" user-emacs-directory))
(add-to-list 'load-path local-pkg)
(setq use-package-hook-name-suffix nil)

(straight-use-package 'org)
(require 'org)

(defmacro rh/expand-all-paths (paths)
  "Collect expansion results for all paths and return them as a
flattened sequence"
  `(flatten-list (cl-mapcar 'file-expand-wildcards ,paths)))

(defun rh/load-maybe-tangled (file)
  "Given FILE load the file, untangling as needed (if it ends with .org)"
  (when-let*
      ((extn (file-name-extension file))
       (load-fn (cond ((string-equal extn "el")
                       (function load-file))
                      ((string-equal extn "org")
                       (function org-babel-load-file)))))
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
