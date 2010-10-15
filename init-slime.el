(update-load-path-vc "slime" t)
(update-load-path-vc "slime/contrib")
(update-load-path-vc "clojure-mode" t)

(require 'starter-kit-lisp)

(defun load-user-clj ()
  (swank-eval (format "(load-file \"%s\")"
                      (expand-file-name (concat dotfiles-dir "user.clj")))))

(add-hook 'slime-connected-hook
          'load-user-clj)

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

(provide 'init-slime)
