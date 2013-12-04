(ulp-site "point-stack" nil t)
(ulp-site "ethan-wspace/lisp" nil 'ethan-wspace)
(ulp-site "diff-hl" t)
(ulp-site "smartrep.el")
(ulp-site "robe" t)
(ulp-site "company" t)
(ulp-site "company-inf-ruby")
(update-load-path (expand-file-name "~/vc/commit-patch"))
(require 'commit-patch-buffer)

(setq company-begin-commands '(self-insert-command))

(eval-after-load 'company
  '(progn
     (pushnew 'company-robe company-backends)
     (pushnew 'company-inf-ruby company-backends)))

(eval-after-load 'yasnippet
  '(progn
     (setq yas-snippet-dirs
           (cons (get-site-dir "js-snippets")
                 (delete-if-not (lambda (dir) (file-directory-p dir))
                                   yas-snippet-dirs)))
     (let ((color (face-attribute 'region :background)))
       (defface yas-field-highlight-box
         `((t :box (:line-width -1 :color ,color)))
         "Box the color of region."))
     (put 'yas-field-highlight-face 'face-alias 'yas-field-highlight-box)))

(eval-after-load '.emacs-loaddefs
  '(progn
     (push 'robe-jump-to point-stack-advised-functions)
     (push 'robe-jump-to-module point-stack-advised-functions)
     (point-stack-setup-advices)))

(eval-after-load 'ido-ubiquitous
  '(push '(disable exact "magit-read-rev") ido-ubiquitous-command-overrides))

(dolist (mode '(ruby js2 js coffee html))
  (add-lambda (intern (format "%s-mode-hook" mode))
    (subword-mode 1)))

(defun fold-grep-command ()
  (if (> (length (match-string 1)) fill-column)
      (let ((beg (match-beginning 1))
            (end (match-end 1))
            (beg-show (/ (- fill-column 5) 2)))
        (let ((inhibit-read-only t))
          (put-text-property (+ beg beg-show) (- end (- fill-column beg-show 5))
                             'display " ... ")))))

(font-lock-add-keywords
 'grep-mode '(("^\n\\(grep .*$\\)"
               (1 (progn (fold-grep-command) 'bold)))))

(defun compile-scroll-eob (buffer _status)
  (let ((win (get-buffer-window buffer))
        (current (selected-window)))
    (when win
      (select-window win)
      (with-current-buffer buffer
        (when (> (line-number-at-pos (point-max)) (window-height))
          (goto-char (point-max))
          (recenter (window-height))))
      (select-window current))))

(add-to-list 'compilation-finish-functions 'compile-scroll-eob)

(setenv "PAGER" (executable-find "cat"))

(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)
(put 'font-lock-regexp-grouping-construct 'face-alias 'font-lock-builtin-face)

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'dired-mode-hook 'rspec-dired-mode)
(add-hook 'diff-mode-hook (lambda () (ethan-wspace-mode -1)))

(provide 'devenv)
