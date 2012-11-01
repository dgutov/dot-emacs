(update-load-path "~/emacs-hs" t)
(ulp-site "js2-mode" t)
(ulp-site "haml-mode" t)

(require 'mmm)

(add-lambda 'c-mode-hook
  (setq-local c-basic-offset 2))

(autoload 'ghc-init "ghc" nil t)

(add-lambda 'haskell-mode-hook
  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode)
  (turn-on-font-lock)
  (ghc-init))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle))

(setq *jdh-javadocs*
      (mapcar (lambda (url) `(,url nil t t))
              '("http://java.sun.com/javase/6/docs/api/"
                "http://tomcat.apache.org/tomcat-5.5-doc/servletapi/"
                "http://commons.apache.org/codec/api-release/"
                "http://commons.apache.org/io/api-release/"
                "http://commons.apache.org/lang/api-release/")))

(defun javadocs-refresh (&optional force)
  (interactive "p")
  (if (or force
          (not (file-exists-p javadoc-help-cache-dir)))
      (mapc (lambda (javadoc)
              (jdh-refresh-url (car javadoc)))
            *jdh-javadocs*)))

(eval-after-load 'javadoc-help
  '(javadocs-refresh))

(add-lambda 'js2-mode-hook
  (setq webjump-api-sites '(("jQuery" . "http://api.jquery.com/"))))

(add-lambda 'ruby-mode-hook
  (setq webjump-api-sites '(("Rails" . "http://apidock.com/rails/")
                            ("Ruby"  . "http://apidock.com/ruby/"))))

(add-hook 'js2-mode-hook 'esk-prog-mode-hook)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(font-lock-add-keywords
 'js2-mode `(("\\(function *\\)("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u0192")
                        nil)))))

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

(eval-after-load 'scheme '(require 'quack))

(add-auto-mode 'js2-mode "\\.js\\'")
(add-auto-mode 'ruby-mode "\\.rake\\'" "\\.gemspec\\'" "\\.ru\\'" "\\.prawn\\'"
               "Rakefile\\'" "Gemfile\\'" "Capfile\\'" "Guardfile\\'")
(add-auto-mode 'markdown-mode "\\.md\\'")
(add-auto-mode 'yaml-mode "\\.yml\\'")

(dolist (mode '(emacs-lisp clojure sldb))
  (add-hook (intern (format "%s-mode-hook" mode))
            (lambda () (autopair-mode -1))))

(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)

(defadvice* check-last-command around (ruby-electric-space-can-be-expanded-p
                                       ruby-electric-return-can-be-expanded-p)
  (when (memq last-command '(self-insert-command undo))
    ad-do-it))

(defadvice ruby-indent-line (after line-up-args activate)
  (let (indent prev-indent arg-indent)
    (save-excursion
      (back-to-indentation)
      (when (zerop (car (syntax-ppss)))
        (setq indent (current-column))
        (skip-chars-backward " \t\n")
        (when (eq ?, (char-before))
          (ruby-backward-sexp)
          (back-to-indentation)
          (setq prev-indent (current-column))
          (skip-syntax-forward "w_.")
          (skip-chars-forward " ")
          (setq arg-indent (current-column)))))
    (when prev-indent
      (let ((offset (- (current-column) indent)))
        (cond ((< indent prev-indent)
               (indent-line-to prev-indent))
              ((= indent prev-indent)
               (indent-line-to arg-indent)))
        (when (> offset 0) (forward-char offset))))))

(defadvice ruby-indent-line (after deep-indent-dwim activate)
  (let (c paren-column indent-column)
    (save-excursion
      (back-to-indentation)
      (save-excursion
        (let ((state (syntax-ppss)))
          (unless (zerop (car state))
            (goto-char (cadr state))
            (setq c (char-after))
            (setq paren-column (current-column))
            (when (memq c '(?{ ?\())
              (forward-char)
              (skip-syntax-forward " ")
              (unless (or (eolp) (eq (char-after) ?|))
                (setq indent-column (current-column)))))))
      (when (and indent-column
                 (eq (char-after) (matching-paren c)))
        (setq indent-column paren-column)))
    (when indent-column
      (let ((offset (- (current-column) (current-indentation))))
        (indent-line-to indent-column)
        (when (> offset 0) (forward-char offset))))))

(eval-after-load 'ruby-mode
  '(remf ruby-deep-indent-paren ?\())

(provide 'progmodes)
