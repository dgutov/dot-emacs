(require 'smex)
(require 'redo+)
(require 'iflipb)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c C-v") 'eval-region)

(global-set-key (kbd "C-x 2") 'split-window-vertically-1)
(global-set-key (kbd "C-x 3") 'split-window-horizontally-1)
(global-change-key (kbd "C-x <up>") 'windmove-up)
(global-change-key (kbd "C-x <down>") 'windmove-down)
(global-change-key (kbd "C-x <left>") 'windmove-left)
(global-change-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "<C-tab>") 'iflipb-next-buffer)
(global-set-key (kbd "<C-S-tab>") 'iflipb-previous-buffer)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "<C-left>") 'backward-word)
(global-set-key (kbd "<C-right>") 'forward-word)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "<M-S-backspace>") 'backward-kill-sexp)
(global-set-key (kbd "<M-S-delete>") 'kill-sexp)
(global-set-key (kbd "<C-delete>") 'kill-word-dwim)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word-dwim)

(global-set-key (kbd "C-/") 'auto-complete)
(global-set-key (kbd "C-x C-n") 'nav)
(global-set-key (kbd "C-c C-g") 'eproject-grep)
(global-set-key (kbd "C-h j") 'javadoc-lookup)

(global-set-key "\C-Y" 'cua-paste-pop)

(global-set-key (kbd "M-<RET>") 'toggle-fs)
(global-set-key (kbd "C-c R") 'rename-current-file-or-buffer)

(global-set-key (kbd "C-x n t") 'nav-tags-here)
(global-set-key (kbd "C-x n b") 'nav-show-bufs)

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "RET") 'dired-find-file-same-buffer)
     (define-key dired-mode-map "^" 'dired-up-directory-same-buffer)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<C-left>") nil)
     (define-key paredit-mode-map (kbd "<C-right>") nil)
     (define-key paredit-mode-map (kbd "<M-left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "<M-backspace>") 'paredit-backward-kill-word)
     (define-key paredit-mode-map (kbd "<M-DEL>") 'paredit-forward-kill-word)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map [home] 'eshell-bol)))

(provide 'init-keys)
