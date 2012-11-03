(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)

(setq autoload-file (expand-file-name "~/.emacs-loaddefs.el")
      locals-file (expand-file-name "~/.emacs-locals.el")
      helm-c-adaptive-history-file "~/.helm-c-adaptive-history"
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
      scroll-conservatively 5
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
      helm-input-idle-delay 0.2
      helm-samewindow t
      css-indent-offset 2
      flyspell-auto-correct-binding [(control ?\')]
      flymake-start-syntax-check-on-find-file nil
      eproject-completing-read-function 'eproject--ido-completing-read
      ido-max-directory-size nil
      quack-remap-find-file-bindings-p nil
      ecb-windows-width 36
      ecb-fix-window-size 'auto
      ecb-layout-name "left15"
      ecb-tip-of-the-day nil
      yas-prompt-functions '(yas/dropdown-prompt)
      yas-expand-only-for-last-commands '(self-insert-command undo)
      yas-verbosity 1
      js-indent-level 2
      js2-basic-offset 2
      js2-enter-indents-newline t
      js2-allow-keywords-as-property-names nil
      js2-move-point-on-right-click nil
      js2-include-rhino-externs nil
      coffee-tab-width 2
      coffee-cleanup-whitespace nil
      autopair-blink nil
      ruby-electric-expand-delimiters-list nil
      gnus-select-method '(nntp "news.gmane.org")
      gnus-interactive-exit nil
      gnus-check-new-newsgroup nil
      gnus-always-read-dribble-file t
      echo-keystrokes 0.02
      smex-flex-matching nil
      undo-no-redo t
      winring-keymap-prefix (kbd "C-x w")
      iedit-toggle-key-default nil
      win-switch-idle-time 10
      win-switch-split-horizontally-keys '("3")
      win-switch-split-vertically-keys '("2")
      win-switch-feedback-background-color "salmon"
      package-enable-at-startup nil)

(setq-default fill-column 80)

(custom-set-variables
 '(help-at-pt-timer-delay 0.1)
 '(help-at-pt-display-when-idle '(flymake-overlay)))

(eval-after-load 'em-prompt
  '(set-face-attribute 'eshell-prompt nil
                       :foreground "SlateBlue4"))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'write-contents-functions
                          (lambda () (delete-trailing-whitespace) nil))))

(add-hook 'emacs-startup-hook
          (lambda () (with-current-buffer "*scratch*"
                  (rename-buffer "-scratch-"))))

(eval-after-load 'package
  '(setq package-archives
         (append package-archives
                 '(("marmalade" . "http://marmalade-repo.org/packages/")
                   ("elpa"      . "http://tromey.com/elpa/")
                   ("melpa"     . "http://melpa.milkbox.net/packages/")))))

(eval-after-load 'finder-inf '(fake-builtins))

(eval-after-load 'starter-kit-defuns
  '(progn
     (remove-hook 'prog-mode-hook 'idle-highlight-mode)
     (remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)))

(defadvice* hide-from-recentf around (ido-save-history update-autoloads)
  (let (write-file-functions
        (find-file-hook (remq 'recentf-track-opened-file find-file-hook)))
    ad-do-it))

(provide 'prefs)
