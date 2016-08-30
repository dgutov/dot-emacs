(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      autoload-file (expand-file-name ".emacs-loaddefs.el"
                                      (concat user-emacs-directory
                                              "site-lisp"))
      locals-file (concat user-emacs-directory ".emacs-locals.el")
      helm-c-adaptive-history-file "~/.helm-c-adaptive-history"
      make-backup-files t
      version-control t
      delete-old-versions t
      auto-save-default nil
      auto-save-list-file-prefix nil
      initial-scratch-message nil
      report-emacs-bug-no-explanations t
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
      recentf-exclude '(file-remote-p)
      save-place-limit 1000
      read-quoted-char-radix 10
      ;; eshell-cmpl-cycle-completions nil
      help-window-select t
      cua-paste-pop-rotate-temporarily t
      cua-enable-cua-keys nil
      org-replace-disputed-keys t
      helm-input-idle-delay 0.2
      helm-split-window-default-side 'below
      helm-autoresize-max-height 45
      helm-autoresize-min-height 25
      helm-split-window-in-side-p t
      css-indent-offset 2
      flyspell-auto-correct-binding [(control ?\')]
      flymake-start-syntax-check-on-find-file nil
      projectile-completion-system 'ivy
      projectile-keymap-prefix (kbd "C-c j")
      ivy-re-builders-alist '((t . ivy--regex-fuzzy))
      ido-max-directory-size nil
      ido-save-directory-list-file "~/.ido.last"
      yas-prompt-functions '(yas-ido-prompt)
      yas-expand-only-for-last-commands '(self-insert-command undo)
      yas-verbosity 1
      js-indent-level 2
      js2-basic-offset 2
      js2-move-point-on-right-click nil
      js2-include-rhino-externs nil
      coffee-tab-width 2
      coffee-cleanup-whitespace nil
      ruby-end-insert-newline nil
      gnus-select-method '(nntp "news.gmane.org")
      gnus-interactive-exit nil
      gnus-check-new-newsgroup nil
      gnus-always-read-dribble-file t
      message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      echo-keystrokes 0.02
      undo-no-redo t
      winring-keymap-prefix (kbd "C-x w")
      iedit-toggle-key-default nil
      package-enable-at-startup nil
      comint-input-ignoredups t
      iflipb-permissive-flip-back t
      split-window-preferred-function 'split-window-prefer-side-by-side
      dired-dwim-target t
      occur-read-regexp-defaults-function 'find-tag-default-as-regexp
      byte-compile--use-old-handlers nil
      easy-kill-try-things '(url email sexp line)
      ffap-url-regexp nil
      mm-discouraged-alternatives '("text/html")
      highlight-tail-steps 8
      highlight-tail-timer 0.05
      company-idle-delay 0.3
      company-transformers '(company-sort-by-occurrence)
      flycheck-rubylintrc "ruby-lint.yml"
      flycheck-disabled-checkers '(ruby-rubylint emacs-lisp-checkdoc ruby-rubocop)
      flycheck-emacs-lisp-initialize-packages nil
      flycheck-emacs-lisp-load-path 'inherit
      aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;)
      sml/theme 'light
      whitespace-cleanup-mode-preserve-point t
      async-bytecomp-allowed-packages nil
      load-prefer-newer t
      history-window-local-history t
      compilation-scroll-output 'first-error
      undo-tree-enable-undo-in-region nil
      ycmd-server-command '("python" "/home/gutov/vc/ycmd/ycmd")
      markdown-command "kramdown"
      inferior-lisp-program "sbcl"
      )

(setq-default indent-tabs-mode nil
              dired-listing-switches "-AlGhv"
              dired-recursive-copies 'always
              bidi-display-reordering nil
              ;; !! electric-indent-inhibit t
              )

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
                 '(;; ("marmalade" . "http://marmalade-repo.org/packages/")
                   ("melpa"     . "http://melpa.milkbox.net/packages/")))))

(defadvice* hide-from-recentf around (ido-save-history update-autoloads)
  (let (write-file-functions
        (find-file-hook (remq 'recentf-track-opened-file find-file-hook)))
    ad-do-it))

(add-lambda 'after-init-hook
  (load-theme 'tango-plus t)

  (custom-theme-set-faces
   'tango-plus
   '(font-lock-comment-face ((t (:foreground "#75507b"))))
   '(font-lock-builtin-face ((t (:foreground "#5c3566"))))
   '(markdown-comment-face ((t (:foreground "dim gray"))))
   '(sml/filename ((t :foreground "#204a87")))))

(eval-after-load 'helm
  '(progn
     (set-face-attribute 'helm-source-header nil :height 1.0 :background nil)
     (helm-autoresize-mode)
     (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
     (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
     (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))
     (defun helm-toggle-header-line ()
       (if (> (length helm-sources) 1)
           (set-face-attribute 'helm-source-header
                               nil
                               :foreground helm-source-header-default-foreground
                               :background helm-source-header-default-background
                               :box helm-source-header-default-box
                               :height 1.0)
         (set-face-attribute 'helm-source-header
                             nil
                             :foreground (face-attribute 'helm-selection :background)
                             :background (face-attribute 'helm-selection :background)
                             :box nil
                             :height 0.1)))
     (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)))

(eval-after-load 'ido-ubiquitous
  '(delete '(enable prefix "xref-") ido-ubiquitous-command-overrides))

(defun scale-default-face ()
  (setq-local face-remapping-alist '((default :height 1.05))))

(add-hook 'minibuffer-setup-hook 'scale-default-face)

(with-current-buffer (get-buffer " *Echo Area 0*")
  (scale-default-face))

(with-current-buffer (get-buffer " *Echo Area 1*")
  (scale-default-face))

(add-hook 'json-mode-hook (lambda () (setq-local js-indent-level 4)))

(eval-after-load 'ivy
  '(add-to-list 'ivy-sort-functions-alist
                '(projectile-switch-project . nil)))

(setq-default right-margin-width 2)

(provide 'prefs)
