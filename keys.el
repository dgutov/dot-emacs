(require 'misc)
(require 'iflipb)
(ulp-site "transpose-frame" nil t)
(require 'winring)
(require 'dired)
(require 'hippie)

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
(global-set-key (kbd "M-l") 'move-to-window-line-top-bottom)
(global-set-key (kbd "<M-S-backspace>") 'backward-kill-sexp)
(global-set-key (kbd "<M-S-delete>") 'kill-sexp)
(global-set-key (kbd "<C-delete>") 'kill-word-dwim)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word-dwim)
(global-set-key (kbd "C-c =") 'align-to-equals)
(global-set-key (kbd "C-c d") 'copy-from-above-command)
(global-set-key (kbd "<M-S-up>") 'move-text-up)
(global-set-key (kbd "<M-S-down>") 'move-text-down)
(global-set-key (kbd "C-x C-;") 'iedit-mode)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-/") 'auto-complete)
(global-set-key (kbd "M-?") 'hippie-expand-undo)
(global-set-key (kbd "C-c s") 'eproject-grep)
(global-set-key (kbd "C-h j") 'javadoc-lookup)
(global-set-key (kbd "C-c a") 'webjump-api)
(global-set-key (kbd "C-;") 'helm-in-project)
(global-set-key (kbd "C-:") 'helm-not-in-project)
(global-set-key (kbd "M-.") 'helm-etags-select-thingatpt)
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

(eval-after-load 'starter-kit-bindings-autoloads
  '(progn
     (global-set-key (kbd "C-x C-i") 'helm-imenu-thingatpt)
     (global-set-key (kbd "C-c f") 'iy-go-to-char)
     (global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
     (dolist (d '(up down left right))
       (win-switch-set-keys `(,(kbd (format "C-<%s>" d))) d)
       (let ((action (eval `(win-switch-dispatch-with
                             ',(intern (format "win-switch-%s" d))))))
         (global-set-key (kbd (format "C-x C-<%s>" d)) action)
         (global-set-key (kbd (format "C-x <%s>" d)) action)))
     (global-set-key (kbd "C-x o") (win-switch-dispatch-with
                                    (lambda (_) (other-window 1))))
     (global-set-key (kbd "C-x p") (win-switch-dispatch-with
                                    (lambda (_) (other-window -1))))))

(eval-after-load 'windmove
  '(defun windmove-default-keybindings ()))

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
     (define-key paredit-mode-map (kbd "<M-left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "<M-backspace>") 'paredit-backward-kill-word)
     (define-key paredit-mode-map (kbd "<M-DEL>") 'paredit-forward-kill-word)))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "<C-tab>") nil))

(eval-after-load 'sgml-mode
  '(progn
     (define-key sgml-mode-map (kbd "C-M-f") 'sgml-skip-tag-forward)
     (define-key sgml-mode-map (kbd "C-M-b") 'sgml-skip-tag-backward)))

(eval-after-load 'undo-tree
  '(progn
     (global-set-key [remap suspend-frame] 'undo-tree-undo)
     (define-key undo-tree-map (kbd "C-S-z") 'undo-tree-redo)
     (define-key undo-tree-map (kbd "C-/") nil)))

(eval-after-load 'ruby-tools
  '(progn
     (define-key ruby-tools-mode-map (kbd "C-S-l") 'ruby-tools-clear-string)
     (define-key ruby-tools-mode-map (kbd "C-;") nil)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map [home] 'eshell-bol)))

(reverse-input-method 'russian-computer)

(find-function-setup-keys)

(provide 'keys)
