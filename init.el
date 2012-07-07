(load (setq custom-file "~/.custom.el") 'noerror)
(when (string-match "mingw-nt" system-configuration)
  (set-face-attribute 'default nil :height 115 :family "Consolas"))

(package-initialize)

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(defun safe-require (package)
  (condition-case err (require package)
    ((debug error) (message "%s" (error-message-string err)))))

(mapc #'safe-require '(defuns prefs keys progmodes devenv))

(if (file-exists-p locals-file) (load locals-file))
(update-autoloads)

(line-number-mode t)
(column-number-mode t)
(winner-mode t)
(cua-mode t)
(autopair-global-mode)
(winring-initialize)
(global-auto-revert-mode t)
(ido-everywhere t)
(global-ethan-wspace-mode t)
(yas/global-mode t)
(savehist-mode t)
(global-diff-hl-mode)
