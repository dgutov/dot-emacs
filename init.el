(load (setq custom-file "~/.custom.el") 'noerror)

(if (string-match "mingw-nt" system-configuration)
    (set-face-attribute 'default nil :height 115 :family "Consolas")
  (set-face-attribute 'default nil :height 105 :family "Source Code Pro")
  (set-fontset-font "fontset-default" 'cyrillic "Dejavu Sans Mono 10"))

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(defun safe-require (package)
  (condition-case err (require package)
    ((debug error) (message "%s" (error-message-string err)))))

(mapc #'safe-require '(defuns prefs keys progmodes mmm devenv))

(package-initialize)

(if (file-exists-p locals-file) (load locals-file))
(update-autoloads)

(line-number-mode)
(column-number-mode)
(winner-mode)
(cua-mode)
(autopair-global-mode)
(winring-initialize)
(global-auto-revert-mode)
(ido-everywhere)
(global-ethan-wspace-mode)
(yas-global-mode)
(savehist-mode)
(global-diff-hl-mode)
(global-undo-tree-mode)
(global-company-mode)
