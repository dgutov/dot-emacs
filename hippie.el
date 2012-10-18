(require 'hippie-exp)

(defvar he-undo-stack nil)

(defun hippie-expand-undo ()
  (interactive)
  (setq this-command 'hippie-expand)
  (if (eq last-command this-command)
      (when he-undo-stack
        (if (cdr he-undo-stack)
            (undo)
          (hippie-expand -1))
        (let ((cell (pop he-undo-stack)))
          (setq he-tried-table (car cell)
                he-num (cdr cell))))
    (setq he-undo-stack nil)))

(defadvice hippie-expand (around expand-lisp-in-minibuffer activate)
  (let ((hippie-expand-try-functions-list
         (if (and (window-minibuffer-p (selected-window))
                  (not (eq major-mode 'emacs-lisp-mode)))
             (hippie-try-list-with-lisp)
           hippie-expand-try-functions-list))
        (hippie-expand-only-buffers (list major-mode)))
    (unless (eq last-command this-command)
      (setq he-undo-stack nil))
    (when (or (not (integerp arg)) (plusp arg))
      (push (cons he-tried-table he-num) he-undo-stack))
    ad-do-it))

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
