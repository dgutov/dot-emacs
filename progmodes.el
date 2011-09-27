(update-load-path "~/emacs-hs" t)
(update-load-path-vc "js2-mode" t)
(update-load-path-vc "autopair")
(update-load-path-vc "slime" t)
(update-load-path-vc "slime/contrib")
(update-load-path-vc "clojure-mode" t)
(update-load-path-vc "rinari" t)
(update-load-path "~/ecb-snap")

(or (require 'yasnippet-bundle nil t)
    (message "Yasnippet bundle not found!"))
(require 'autopair)
(require 'ecb-autoloads)
(require 'mmm)

(add-lambda 'c-mode-hook
  (setq-local c-basic-offset 2))

(autoload 'ghc-init "ghc" nil t)

(add-lambda 'haskell-mode-hook
  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode)
  (turn-on-font-lock)
  (ghc-init))

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
                      (expand-file-name (concat user-emacs-directory
                                                "user.clj")))))

(add-hook 'slime-connected-hook
          'load-user-clj)

(add-lambda 'slime-repl-mode-hook
  (set-syntax-table clojure-mode-syntax-table)
  (define-key slime-repl-mode-map (kbd "{") 'paredit-open-curly)
  (define-key slime-repl-mode-map (kbd "}") 'paredit-close-curly)
  (paredit-mode t))

(eval-after-load 'slime
  '(progn
     (slime-setup '(slime-fancy slime-banner slime-repl))
     (setq slime-startup-animation nil)
     (setq slime-protocol-version 'ignore)))

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

(add-auto-mode 'ruby-mode "\\.rake$" "\\.gemspec$" "\\.ru$"
               "Rakefile$" "Gemfile$" "Capfile$")
(add-auto-mode 'python-mode "SConstruct" "SConscript")
(add-auto-mode 'markdown-mode "\\.md$")

(dolist (mode '(emacs-lisp clojure slime-repl sldb))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            (lambda () (setq autopair-dont-activate t))))

(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(defadvice* check-last-command around (ruby-electric-space-can-be-expanded-p
                                       ruby-electric-return-can-be-expanded-p)
  (when (memq last-command '(self-insert-command undo))
    ad-do-it))

(provide 'progmodes)
