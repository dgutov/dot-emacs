(ulp-site "js2-mode" t)
(ulp-site "rspec-mode" t)

(add-lambda 'c-mode-hook
  (setq-local c-basic-offset 2))

(autoload 'ghc-init "ghc" nil t)

(add-lambda 'haskell-mode-hook
  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode)
  (turn-on-font-lock)
  ;; (ghc-init)
  )

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle))

(add-hook 'coffee-mode-hook 'flymake-coffee-load)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(eval-after-load 'js2-mode
  '(js2-imenu-extras-setup))

(add-lambda 'slime-repl-mode-hook
  (set-syntax-table clojure-mode-syntax-table)
  (define-key slime-repl-mode-map (kbd "{") 'paredit-open-curly)
  (define-key slime-repl-mode-map (kbd "}") 'paredit-close-curly)
  (paredit-mode t))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map [f5] 'slime-connect)
     (define-clojure-indent
       ;; clojure.test
       (thrown? 1)
       ;; compojure
       (html 0)
       (xhtml-tag 1)
       (GET 2)
       (POST 2)
       (PUT 2)
       (DELETE 2)
       (ANY 2))))

(add-auto-mode 'js2-mode "\\.js\\'")
(add-auto-mode 'ruby-mode "\\.rake\\'" "\\.ru\\'" "\\.prawn\\'"
               "Gemfile\\'" "Capfile\\'" "Guardfile\\'")
(add-auto-mode 'markdown-mode "\\.md\\'")
(add-auto-mode 'yaml-mode "\\.yml\\'")

(dolist (mode '(emacs-lisp clojure sldb))
  (add-hook (intern (format "%s-mode-hook" mode))
            (lambda () (autopair-mode -1))))

(add-hook 'ruby-mode-hook 'robe-mode)

(defadvice* check-last-command around (ruby-electric-space-can-be-expanded-p
                                       ruby-electric-return-can-be-expanded-p)
  (when (memq last-command '(self-insert-command undo))
    ad-do-it))

(eval-after-load 'inf-ruby '(inf-ruby-switch-setup))

(defadvice flymake-parse-residual (after clear-ruby-warnings () activate)
  (setq flymake-new-err-info
        (delete-if (lambda (item) (string-match "ambiguous first argument; put"
                                           (flymake-ler-text (caadr item))))
                   flymake-new-err-info)))

(eval-after-load 'ruby-mode
  '(progn
     (setq ruby-deep-indent-paren (delete ?\( ruby-deep-indent-paren))
     (define-key ruby-mode-map (kbd "C-c :") 'ruby-toggle-hash-syntax)
     (require 'ruby-end)))

(when (string-lessp "24.3.50" emacs-version)
  (eval-after-load 'rspec-mode
    '(rspec-install-snippets)))

(add-lambda 'prog-mode-hook
  (when buffer-file-name (hack-local-variables))
  (whitespace-mode 1))
(add-hook 'html-mode-hook 'whitespace-mode)

(dolist (mode '(emacs-lisp clojure js2 js))
  (add-hook (intern (format "%s-mode-hook" mode))
            (lambda ()
              (add-hook 'after-save-hook 'check-parens nil t))))

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(provide 'progmodes)
