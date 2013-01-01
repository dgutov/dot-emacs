(require 'hippie-exp)

(defvar he-try-list-tail '(try-expand-dabbrev-all-buffers
                           try-complete-file-name-partially
                           try-complete-file-name))

(setq hippie-expand-try-functions-list
      (cons 'try-expand-dabbrev he-try-list-tail))

(defun hippie-try-list-with-lisp ()
  (append '(try-expand-dabbrev
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol)
          hippie-expand-try-functions-list))

(add-lambda 'emacs-lisp-mode-hook
  (set (make-local-variable 'hippie-expand-try-functions-list)
       (hippie-try-list-with-lisp)))

(provide 'hippie)
