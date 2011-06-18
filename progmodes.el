(update-load-path-vc "emacs-nav" t)
(update-load-path "~/emacs-hs" t)
(update-load-path-vc "js2-mode" t)
(update-load-path-vc "autopair")
(update-load-path-vc "slime" t)
(update-load-path-vc "slime/contrib")
(update-load-path-vc "clojure-mode" t)
(update-load-path-vc "rhtml")

(require 'starter-kit-lisp)
(or (require 'yasnippet-bundle nil t)
    (message "Yasnippet bundle not found!"))
(require 'autopair)

(add-hook 'c-mode-hook
          (lambda ()
            (setq-local c-basic-offset 2)))

(autoload 'ghc-init "ghc" nil t)

(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-indentation)
            (turn-on-haskell-doc-mode)
            (turn-on-font-lock)
            (ghc-init)))

(setq *jdh-javadocs*
      (mapcar (lambda (url) `(,url nil t t))
              '("http://java.sun.com/javase/6/docs/api/"
                "http://java.sun.com/products/servlet/2.5/docs/servlet-2_5-mr2/"
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

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle))

(eval-after-load 'javadoc-help
  '(javadocs-refresh))

(add-hook 'js2-mode-hook 'run-coding-hook)

(setq js2-basic-offset 2
      js2-auto-indent-p t
      js2-consistent-level-indent-inner-bracket-p t
      js2-use-ast-for-indentation-p t
      js2-enter-indents-newline t
      js2-allow-keywords-as-property-names nil
      js2-move-point-on-right-click nil
      autopair-blink nil)

(defun load-user-clj ()
  (swank-eval (format "(load-file \"%s\")"
                      (expand-file-name (concat dotfiles-dir "user.clj")))))

(add-hook 'slime-connected-hook
          'load-user-clj)

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (set-syntax-table clojure-mode-syntax-table)
            (define-key slime-repl-mode-map (kbd "{") 'paredit-open-curly)
            (define-key slime-repl-mode-map (kbd "}") 'paredit-close-curly)
            (paredit-mode t)))

(defun slime-next-sexp ()
  (save-excursion
    (forward-sexp)
    (buffer-substring-no-properties
     (save-excursion
       (backward-sexp)
       (point))
     (point))))

(defun swank-eval (string)
  (slime-eval `(swank:interactive-eval ,string)))

(defun clojure-next-sexp-class ()
  (interactive)
  (message
   (swank-eval (format "(user/expression-classname `%s)" (slime-next-sexp)))))

(defun clojure-next-sexp-javadoc ()
  (interactive)
  (swank-eval (format "(user/expression-javadoc `%s)" (slime-next-sexp))))

(eval-after-load 'slime
  '(progn
     (slime-setup '(slime-fancy slime-banner slime-repl))
     (setq slime-startup-animation nil)
     (setq slime-protocol-version 'ignore)
     (define-key slime-mode-map (kbd "C-c d") 'clojure-next-sexp-class)
     (define-key slime-mode-map (kbd "C-c D") 'clojure-next-sexp-javadoc)))

(eval-after-load 'slime-repl
  '(progn
     (define-key slime-repl-mode-map (kbd "C-c d") 'clojure-next-sexp-class)
     (define-key slime-repl-mode-map (kbd "C-c D") 'clojure-next-sexp-javadoc)))

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

(add-auto-mode 'rhtml-mode "\.html\.erb$")
(autoload 'rhtml-mode "rhtml-mode" nil t)

(add-hook 'snippet-mode-hook
          (lambda () (add-hook 'write-contents-functions
                          'delete-trailing-whitespace-and-newlines)))

(dolist (mode '(emacs-lisp clojure slime-repl sldb))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            (lambda () (setq autopair-dont-activate t))))

(provide 'progmodes)
