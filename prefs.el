(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq autoload-file (expand-file-name "~/.emacs-loaddefs.el")
      locals-file (expand-file-name "~/.emacs-locals.el")
      anything-c-adaptive-history-file "~/.anything-c-adaptive-history"
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
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      truncate-lines t
      kill-whole-line t
      mouse-wheel-scroll-amount '(3)
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      compilation-window-height 17
      recentf-max-saved-items 1000
      save-place-limit 1000
      read-quoted-char-radix 10
      eshell-cmpl-cycle-completions nil
      redisplay-dont-pause t
      help-window-select t
      cua-paste-pop-rotate-temporarily t
      cua-enable-cua-keys nil
      org-replace-disputed-keys t
      vc-handled-backends (delq 'Git vc-handled-backends) ;; vc-git is slow
      anything-input-idle-delay 0.2
      anything-samewindow t
      css-indent-offset 2
      flyspell-auto-correct-binding [(control ?\')]
      flymake-start-syntax-check-on-find-file nil
      eproject-completing-read-function 'eproject--ido-completing-read
      quack-remap-find-file-bindings-p nil
      ecb-windows-width 36
      ecb-fix-window-size 'auto
      ecb-layout-name "left15"
      ecb-tip-of-the-day nil
      yas/prompt-functions '(yas/dropdown-prompt)
      yas/expand-only-for-last-commands '(self-insert-command undo)
      mmm-global-mode 'buffers-with-submode-classes
      mmm-submode-decoration-level 2
      js-indent-level 2
      js2-basic-offset 2
      js2-enter-indents-newline t
      js2-allow-keywords-as-property-names nil
      js2-move-point-on-right-click nil
      autopair-blink nil
      ruby-electric-expand-delimiters-list nil
      gnus-select-method '(nntp "news.gmane.org")
      gnus-interactive-exit nil
      echo-keystrokes 0.02)

(setq-default fill-column 80)

(eval-after-load 'esh-opt
  '(set-face-attribute 'eshell-prompt nil
                       :foreground "SlateBlue4"))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'write-contents-functions
                          (lambda () (delete-trailing-whitespace) nil))))

(add-hook 'emacs-startup-hook
          (lambda () (with-current-buffer "*scratch*"
                  (rename-buffer "-scratch-"))))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)

(defadvice* hide-from-recentf around (ido-save-history update-autoloads)
  (let (write-file-functions
        (find-file-hook (remq 'recentf-track-opened-file find-file-hook)))
    ad-do-it))

(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)

(provide 'prefs)
