(ulp-site "point-stack" nil t)
(ulp-site "diff-hl" t)
(ulp-site "smartrep.el")
(ulp-site "robe" t)
(ulp-site "company" t)
(ulp-site "company-inf-ruby")
(update-load-path (expand-file-name "~/vc/commit-patch"))
(update-load-path (expand-file-name "~/vc/emacs-ycmd"))
(update-load-path (expand-file-name "~/vc/company-anaconda"))
(update-load-path (expand-file-name "~/vc/magit/lisp"))
(require 'commit-patch-buffer)

;; (eval-after-load 'company
;;   '(progn
;;      (push '(company-robe :with company-yasnippet) company-backends)))

(eval-after-load 'company
  '(cl-pushnew 'company-robe company-backends))

(eldoc-add-command 'ruby-end-return)

(eval-after-load 'yasnippet
  '(progn
     (setq yas-snippet-dirs
           (cons (get-site-dir "js-snippets")
                 (cl-delete-if-not (lambda (dir)
                                  (when (symbolp dir)
                                    (setq dir (symbol-value dir)))
                                  (file-directory-p dir))
                                   yas-snippet-dirs)))
     (let ((color (face-attribute 'region :background)))
       (defface yas-field-highlight-box
         ;; set it to (-1 . -1) for Emacs 28 later
         `((t :box (:line-width -1 :color ,color)))
         "Box the color of region."))
     (put 'yas-field-highlight-face 'face-alias 'yas-field-highlight-box)))

(eval-after-load '.emacs-loaddefs
  '(progn
     (push 'robe-jump-to point-stack-advised-functions)
     (push 'robe-jump-to-module point-stack-advised-functions)
     ;;(point-stack-setup-advices)
     ))

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

(eval-after-load 'ansi-color
  '(progn
     (aset ansi-color-names-vector 2 "dark green")
     (setq ansi-color-map (ansi-color-make-color-map))))

(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)
(put 'font-lock-regexp-grouping-construct 'face-alias 'font-lock-builtin-face)

(add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
(add-hook 'dired-mode-hook 'rspec-dired-mode)

(defvar history-advised-functions
  '(isearch-mode find-function-do-it find-library
    imenu beginning-of-buffer end-of-buffer
    xref-find-definitions counsel-imenu counsel-git-grep
    xref-find-references
    paredit-backward-up backward-up-list))

(defun history-add-history-etc (&rest _ignore)
  (history-add-history))

(dolist (fun history-advised-functions)
  (advice-add fun :before
              #'history-add-history-etc))

(provide 'devenv)
