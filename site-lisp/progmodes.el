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

(add-hook 'ruby-mode-hook 'flymake-mode)

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

(defadvice* check-last-command around (ruby-electric-space-can-be-expanded-p
                                       ruby-electric-return-can-be-expanded-p)
  (when (memq last-command '(self-insert-command undo))
    ad-do-it))

(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)

(eval-after-load 'ruby-mode
  '(progn
     (setq ruby-deep-indent-paren (delete ?\( ruby-deep-indent-paren))
     (define-key ruby-mode-map (kbd "C-c :") 'ruby-toggle-hash-syntax)
     (require 'ruby-end)))

(when (string-lessp "24.3.50" emacs-version)
  (eval-after-load 'rspec-mode
    '(progn
       (rspec-install-snippets)
       (inf-ruby-switch-setup))))

(update-load-path "~/vc/elixir-mode")
(autoload 'elixir-mode "elixir-mode" nil t)
(add-auto-mode 'elixir-mode "\\.exs?\\'")

(eval-after-load 'elixir-mode
  '(progn
     (define-key elixir-mode-map (kbd "C-c ,a") 'exunit-verify-all)
     (define-key elixir-mode-map (kbd "C-c ,v") 'exunit-verify)
     (define-key elixir-mode-map (kbd "C-c ,r") 'exunit-rerun)
     (define-key elixir-mode-map (kbd "C-c ,s") 'exunit-verify-single)
     (define-key elixir-mode-map (kbd "C-c ,t") 'exunit-toggle-file-and-test)
     ))

(eval-after-load 'eglot
  '(progn
     (add-to-list 'eglot-server-programs
                  `(elixir-mode ,(get-vc-dir "elixir-ls/release/language_server.sh")))
     (define-key eglot-mode-map (kbd "C-c C-d") #'eglot-help-at-point)))

(add-lambda 'prog-mode-hook
  (add-hook 'hack-local-variables-hook #'whitespace-mode t t))

(add-hook 'html-mode-hook 'whitespace-mode)

(dolist (mode '(emacs-lisp clojure js2 js))
  (add-hook (intern (format "%s-mode-hook" mode))
            (lambda ()
              (add-hook 'after-save-hook 'check-parens nil t))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (remove-hook 'flymake-diagnostic-functions 'elisp-flymake-checkdoc t)
            (flymake-mode)))

(eval-after-load 'eldoc
  '(eldoc-add-command 'ruby-end-space 'paredit-backward-delete
                      'electric-pair-backward-delete-char))

(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

(provide 'progmodes)
