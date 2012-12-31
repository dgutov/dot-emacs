(ulp-site "eproject" nil t)
(ulp-site "point-stack" nil t)
(ulp-site "ethan-wspace/lisp" nil 'ethan-wspace)
(ulp-site "diff-hl" t)
(ulp-site "smartrep.el")
(ulp-site "robe" t)
(ulp-site "company")

(require 'eproject-extras)
(require 'company)

(define-project-type emacs (generic) (look-for "init.el")
  :irrelevant-files ("/elpa/" "/url/cookies$" "tramp$" "/server/" "history$"
                     "^custom.el$" "^places$" "/backups/" "/site-lisp/.*/"))

(eval-after-load 'company
  '(progn
     (set-face-attribute 'company-tooltip nil
                         :background "cornsilk")
     (set-face-attribute 'company-tooltip-common nil
                         :foreground "darkred")
     (set-face-attribute 'company-tooltip-common-selection nil
                         :foreground "darkred")
     (set-face-attribute 'company-tooltip-selection nil
                         :background "light blue")))

(setq company-begin-commands '(self-insert-command)
      company-backends '(company-elisp company-robe company-css
                         (company-etags company-dabbrev-code company-keywords)
                         company-files company-dabbrev))

(add-lambda 'minibuffer-setup-hook
  (set (make-local-variable 'company-backends) nil))

(eval-after-load 'semantic
  '(push 'company-semantic company-backends))

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

(add-lambda 'haskell-mode-hook
  (setq-local ac-sources '(ac-source-ghc-mod
                           ac-source-words-in-same-mode-buffers)))

(eval-after-load '.emacs-loaddefs
  '(progn
     (push 'robe-jump-to point-stack-advised-functions)
     (push 'robe-jump-to-module point-stack-advised-functions)
     (point-stack-setup-advices)))

(eval-after-load 'ido-ubiquitous
  '(ido-ubiquitous-disable-in magit-read-rev))

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

(provide 'devenv)
