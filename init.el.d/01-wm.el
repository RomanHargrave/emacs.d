;; Inspired by Dale Sedevic's `my:pop-up-buffer-p'
(defun rh/is-popup-buffer (&optional buffer)
  "Is BUFFER a pop-up buffer?"
  (with-current-buffer (or buffer (current-buffer))
    (derived-mode-p 'compilation-mode
                    'epa-key-list-mode
                    'help-mode
                    'rustfmt)))

(use-package shackle
  :demand t
  :config
  (defvar rh/shackle-defaults
    '(:popup t :align below :size 0.2))
  (setq shackle-rules
        `(('("*Help*" "*General Keybindings*" "*Flycheck errors*" "*Apropos*")
           ,@rh/shackle-defaults
           :select t)
          ('("*rustfmt*") ,@rh/shackle-defaults :select nil)
          ('(:custom rh/is-popup-buffer) ,@rh/shackle-defaults)))
  (shackle-mode 1))

(use-package winum
  :bind (("C-c w w" . 'winum-select-window-by-number)
         ("C-c w d" . 'rh/kill-winum))
  :config
  ;; no, i don't want it. don't force keybindings on your users.
  ;; especially keybindings this shallow. that's my job, fuck off.
  (define-key winum-keymap (kbd "C-x w") nil)
  (winum-mode 1)
  (defun rh/kill-winum (number)
    "Kill window using positive number."
    (interactive "nWindow: ")
    (winum-select-window-by-number (- number))))

(use-package ace-window
  :bind (("C-x o" . 'ace-window)
         ("C-x M-0" . 'ace-delete-window)))

(setq mouse-autoselect-window t)

(general-define-key
 "C-c w q" 'delete-window)
