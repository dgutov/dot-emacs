(require 'misc)
(ulp-site "transpose-frame" nil t)
(require 'winring)
(require 'dired)
(require 'hippie)
(ulp-site "helm" nil 'helm-config)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c C-v") 'eval-region)

(global-set-key (kbd "C-x 2") 'split-window-vertically-1)
(global-set-key (kbd "C-x 3") 'split-window-horizontally-1)
(global-set-key (kbd "C-x 8") 'rotate-windows)
(global-set-key (kbd "C-x t") 'transpose-frame)
(global-set-key (kbd "<C-tab>") 'iflipb-next-buffer)
(global-set-key (kbd "<C-S-tab>") 'iflipb-previous-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'iflipb-previous-buffer)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<C-left>") 'backward-word)
(global-set-key (kbd "<C-right>") 'forward-word)
(global-set-key (kbd "<C-up>") 'backward-sentence)
(global-set-key (kbd "<C-down>") 'forward-sentence)
(global-set-key (kbd "<M-S-backspace>") 'backward-kill-sexp)
(global-set-key (kbd "<M-S-delete>") 'kill-sexp)
(global-set-key (kbd "<C-delete>") 'kill-word-dwim)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word-dwim)
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "C-c =") 'align-to-equals)
(global-set-key (kbd "C-c d") 'copy-from-above-command)
(global-set-key (kbd "<M-S-up>") 'move-text-up)
(global-set-key (kbd "<M-S-down>") 'move-text-down)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x C-;") 'mc/mark-all-like-this-dwim)

(global-set-key (kbd "C-/") 'company-complete)
(global-set-key (kbd "C-h j") 'javadoc-lookup)
(global-set-key (kbd "C-c a") 'webjump-api)
(global-set-key (kbd "C-;") 'helm-in-project)
(global-set-key (kbd "C-:") 'helm-not-in-project)
(global-set-key (kbd "M-.") 'helm-etags-select-thingatpt)
(global-set-key (kbd "C-x C-i") 'helm-imenu)
(global-set-key [remap describe-bindings] 'helm-descbinds)
(global-set-key (kbd "C-M-,") 'point-stack-pop)
(global-set-key (kbd "C-M-.") 'point-stack-forward-stack-pop)
(global-set-key (kbd "C-M-/") 'point-stack-push)

(global-set-key "\M-Y" 'cua-paste-pop)
(global-unset-key (kbd "<S-delete>"))
(global-unset-key (kbd "<S-insert>"))
(global-unset-key (kbd "<C-insert>"))

(global-set-key (kbd "M-<RET>") 'toggle-fs)
(global-set-key (kbd "C-c C-r") 'rename-current-file-or-buffer)

(define-key global-map (kbd "C-x C-d") 'dired)
(define-key global-map (kbd "C-x C-j") 'dired-jump)
(define-key global-map (kbd "C-x 4 C-j") 'dired-jump-other-window)

(define-key vc-prefix-map "d" 'vc-dir-quick)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(windmove-default-keybindings 'meta)
(global-set-key (kbd "M-]") 'other-window)
(global-set-key (kbd "M-[") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x o") 'switch-window)

(define-key isearch-mode-map (kbd "M-w") 'isearch-yank-symbol-at-point)

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "RET") 'dired-find-file-same-buffer)
     (define-key dired-mode-map "^" 'dired-up-directory-same-buffer)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<C-left>") nil)
     (define-key paredit-mode-map (kbd "<C-right>") nil)
     (define-key paredit-mode-map (kbd "M-s") nil)
     (define-key paredit-mode-map (kbd "M-S") nil)
     (define-key paredit-mode-map (kbd "<M-up>") nil)
     (define-key paredit-mode-map (kbd "<M-down>") nil)
     (define-key paredit-mode-map (kbd "M-i") 'paredit-splice-sexp)
     (define-key paredit-mode-map (kbd "<M-backspace>") 'paredit-backward-kill-word)
     (define-key paredit-mode-map (kbd "<M-DEL>") 'paredit-forward-kill-word)))

(eval-after-load 'org
  '(progn (define-key org-mode-map (kbd "<C-tab>") nil)
          (define-key org-mode-map (kbd "<M-up>") nil)
          (define-key org-mode-map (kbd "<M-down>") nil)
          (define-key org-mode-map (kbd "<M-left>") nil)
          (define-key org-mode-map (kbd "<M-right>") nil)))

(eval-after-load 'markdown-mode
  '(progn (define-key markdown-mode-map (kbd "<M-up>") nil)
          (define-key markdown-mode-map (kbd "<M-down>") nil)
          (define-key markdown-mode-map (kbd "<M-left>") nil)
          (define-key markdown-mode-map (kbd "<M-right>") nil)))

(eval-after-load 'sgml-mode
  '(progn
     (define-key sgml-mode-map (kbd "C-M-f") 'sgml-skip-tag-forward)
     (define-key sgml-mode-map (kbd "C-M-b") 'sgml-skip-tag-backward)))

(eval-after-load 'undo-tree
  '(progn
     (global-set-key [remap suspend-frame] 'undo-tree-undo)
     (define-key undo-tree-map (kbd "C-S-z") 'undo-tree-redo)
     (define-key undo-tree-map (kbd "C-/") nil)))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-c C-d") 'company-show-doc-buffer)
     (define-key company-active-map (kbd "C-o") 'ignore)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "C-/") 'company-complete-common)))

(eval-after-load 'gnus-sum
  '(progn
     (define-key gnus-summary-mode-map [(meta down)] nil)
     (define-key gnus-summary-mode-map [(meta up)] nil)))

(add-lambda 'eshell-mode-hook
  (define-key eshell-mode-map [home] 'eshell-bol))

(autoload 'switch-window "switch-window")

(reverse-input-method 'russian-computer)

(find-function-setup-keys)

(provide 'keys)
