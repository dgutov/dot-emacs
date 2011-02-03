(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(require 'saveplace)

(setq autoload-file (expand-file-name "~/.emacs-loaddefs.el")
      save-place-file (expand-file-name "~/.emacs-places.el")
      locals-file (expand-file-name "~/.emacs-locals.el")
      anything-c-adaptive-history-file "~/.anything-c-adaptive-history"
      backup-directory-alist `(("." . ,(expand-file-name (concat dotfiles-dir ".backups"))))
      indent-tabs-mode nil
      make-backup-files t
      version-control t
      delete-old-versions t
      auto-save-default nil
      auto-save-list-file-prefix nil
      initial-scratch-message nil
      shift-select-mode t
      auto-fill-mode nil
      default-major-mode 'text-mode
      initial-major-mode 'emacs-lisp-mode
      scroll-conservatively most-positive-fixnum
      scroll-step 1
      scroll-margin 0
      scroll-preserve-screen-position t
      truncate-lines t
      kill-whole-line t
      mouse-wheel-scroll-amount '(3)
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      compilation-window-height 17
      recentf-max-saved-items 100
      save-place-limit 100
      read-quoted-char-radix 10
      eshell-cmpl-cycle-completions nil
      redisplay-dont-pause t
      help-window-select t
      fill-column 80
      cua-paste-pop-rotate-temporarily t
      cua-prefix-override-inhibit-delay 0.05
      yas/wrap-around-region 'cua
      vc-handled-backends (delq 'Git vc-handled-backends) ;; vc-git is slow
      anything-input-idle-delay 0.2
      anything-samewindow t
      flyspell-auto-correct-binding [(control ?\')]
      eproject-completing-read-function 'eproject--ido-completing-read)

(eval-after-load 'esh-opt
  '(set-face-attribute 'eshell-prompt nil
                       :foreground "SlateBlue4"))

(line-number-mode t)
(column-number-mode t)
(winner-mode t)
(cua-mode t)
(autopair-global-mode)

(provide 'init-misc)
