;;; Pillaged from emacs-starter-kit.

(tooltip-mode -1)
(blink-cursor-mode -1)

(require 'uniquify)

(setq visible-bell t
      inhibit-startup-message t
      sentence-end-double-space nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face lines-tail tabs trailing)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      diff-switches "-u")

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-everywhere t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-max-prospects 10)

(setq-default imenu-auto-rescan t
              indicate-empty-lines t)

(eval-after-load "ispell"
  '(when (executable-find ispell-program-name)
   (add-hook 'text-mode-hook 'turn-on-flyspell)))

(defalias 'yes-or-no-p 'y-or-n-p)

(random t)

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-added "green4")
     (set-face-foreground 'magit-diff-removed "red3")))

(defun esk-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun esk-turn-on-save-place-mode ()
  (require 'saveplace)
  (setq save-place t))

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
(add-hook 'prog-mode-hook 'esk-turn-on-save-place-mode)
(add-hook 'prog-mode-hook 'esk-pretty-lambdas)
(add-hook 'prog-mode-hook 'esk-add-watchwords)

(dolist (mode '(js js2))
  (font-lock-add-keywords (intern (format "%s-mode" mode))
                          `(("\\(function\\) *("
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "\u0192")
                                       nil)))))
  (add-hook (intern (format "%s-mode-hook" mode))
            (lambda ()
              (make-local-variable 'font-lock-extra-managed-props)
              (push 'composition font-lock-extra-managed-props))))

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

(defface esk-paren-face
  '((((class color) (background light))
     (:foreground "grey55")))
  "Face for parens.")

(dolist (mode '(scheme emacs-lisp lisp lisp-interaction clojure clojurescript
                       inferior-emacs-lisp nrepl))
  (when (> (display-color-cells) 8)
    (font-lock-add-keywords (intern (format "%s-mode" mode))
                            '(("(\\|)" . 'esk-paren-face))))
  (add-hook (intern (format "%s-mode-hook" mode)) 'paredit-mode))

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(define-key 'help-command "a" 'apropos)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2)))
(global-set-key (kbd "C-c x") 'execute-extended-command)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

(provide 'esk)
