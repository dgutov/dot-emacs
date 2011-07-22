(defmacro setq-local (var value)
  "Makes VAR buffer-local and sets its value."
  `(set (make-local-variable ',var) ,value))

(defun filter (condp list)
  "Returns new list containing all elements of LIST that satisfy CONDP."
  (let* ((result (cons nil nil))
         (cell result))
    (while list
      (let ((el (car list)))
        (if (funcall condp el)
            (setq cell (setcdr cell (cons el nil)))))
      (setq list (cdr list)))
    (cdr result)))

(defvar autoload-directory-list nil
  "List of directories to generate autoloads from.")

(defun update-autoloads (&optional force-regen)
  (interactive "p")
  (let ((generated-autoload-file autoload-file)
        (dirs (if (or force-regen (not (file-exists-p autoload-file)))
                  autoload-directory-list
                (filter (lambda (directory)
                          (some (lambda (f) (file-newer-than-file-p f autoload-file))
                                (directory-files directory t "\\.el$")))
                        autoload-directory-list))))
    (dolist (dir dirs)
      (message "Updating autoloads from %s" dir)
      (let (emacs-lisp-mode-hook)
        (update-directory-autoloads dir)))
    (load autoload-file)))

(defun update-load-path (directory &optional add-to-autoloads)
  (add-to-list 'load-path directory)
  (when add-to-autoloads (add-to-list 'autoload-directory-list directory)))

(defun get-vc-dir (dir-name)
  (expand-file-name (concat "~/vc/" dir-name)))

(defun update-load-path-vc (dir-name &optional add-to-autoloads)
  (update-load-path (get-vc-dir dir-name) add-to-autoloads))

(defun add-auto-mode (mode &rest patterns)
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun global-change-key (key command)
  (mapc #'global-unset-key (where-is-internal command))
  (global-set-key key command))

(defadvice apropos (after select-window (pattern &optional do-all) activate)
  "Selects apropos window and makes it dedicated."
  (let ((window (get-buffer-window "*Apropos*")))
    (when window
      (select-window window)
      (set-window-dedicated-p window t)))) ;; makes 'Q' delete the window

(defadvice delete-window (around select-window-at-coords (&optional window) activate)
  "Selects window that takes space of the deleted one."
  (let ((coords (subseq (window-edges window) 0 2)))
    ad-do-it
    (select-window (apply 'window-at coords))))

(defun split-window-vertically-1 ()
  "Splits window vertically and selects the new window."
  (interactive)
  (select-window (split-window-vertically)))

(defun split-window-horizontally-1 ()
  "Splits window horizontally and selects the new window."
  (interactive)
  (select-window (split-window-horizontally)))

(defmacro whack-skipped-chars (&rest forms)
  `(let ((start (point))
         (skip-chars-fn (if backward
                            'skip-chars-backward
                          'skip-chars-forward)))
     ,@forms
     (prog1 (abs (- (point) start))
       (delete-region start (point)))))

(defun whack-whitespace (delete-newlines &optional backward)
  "Deletes all whitespace before or after the point.
If DELETE-NEWLINES is true, it first deletes the newlines after the point.
Returns the deleted character count."
  (whack-skipped-chars
   (if delete-newlines
       (funcall skip-chars-fn "\n"))
   (funcall skip-chars-fn " \t")))

(defun whack-nonchars (&optional backward)
  "Deletes all visible non-word characters before or after the point.
Returns the deleted character count."
  (whack-skipped-chars
   (funcall skip-chars-fn "[:punct:]")))

(defun kill-word-dwim (arg)
  (interactive "p")
  (when (zerop (whack-whitespace t))
    (if (zerop (whack-nonchars))
        (kill-word arg))
    (whack-whitespace nil)))

(defun backward-kill-word-dwim (arg)
  (interactive "p")
  (when (zerop (whack-whitespace t t))
    (if (zerop (whack-nonchars t))
        (backward-kill-word arg))))

(if (fboundp 'w32-send-sys-command)
    (progn
      (defsubst toggle-fs-on ()
        (w32-send-sys-command 61488))
      (defsubst toggle-fs-off ()
        (w32-send-sys-command 61728)))
  (progn
    (defsubst toggle-fs-on ()
      (set-frame-parameter nil 'fullscreen 'fullboth))
    (defsubst toggle-fs-off ()
      (set-frame-parameter nil 'fullscreen nil))))

(defun toggle-fs ()
  (interactive)
  (if (frame-parameter nil 'fs)
      (progn (set-frame-parameter nil 'fs nil)
             (toggle-fs-off))
    (progn (set-frame-parameter nil 'fs t)
           (toggle-fs-on))))

(defun dired-find-file-same-buffer ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (find-alternate-file file)
      (find-file file))))

(defun dired-up-directory-same-buffer ()
  (interactive)
  (find-alternate-file ".."))

(defun rename-current-file-or-buffer ()
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)

(defadvice nav-mode (after window-height-fixed () activate)
  (setq window-size-fixed 'height))

(defun compile-snippets ()
  (interactive)
  (update-load-path-vc "yasnippet")
  (require 'yasnippet)
  (let ((bundle-file (expand-file-name (concat dotfiles-dir "site-lisp/yasnippet-bundle.el"))))
    (yas/compile-bundle (get-vc-dir "yasnippet/yasnippet.el")
                        bundle-file
                        `(,(get-vc-dir "yasnippet/snippets")
                          ,(get-vc-dir "js-yasnippets"))
                        "(yas/initialize-bundle)\n;;;### autoload(require 'yasnippet-bundle)"
                        (get-vc-dir "yasnippet/dropdown-list.el"))
    (byte-compile-file bundle-file)))

(defun delete-trailing-whitespace-and-newlines ()
  (interactive)
  (save-excursion
    (end-of-buffer)
    (delete-trailing-whitespace)
    (delete-blank-lines)
    (if (looking-at "^")
        (delete-backward-char 1))))

(defun rotate-windows ()
  (interactive)
  (let* ((windows (window-list))
         (buffers (mapcar 'window-buffer windows))
         (windows (nconc (cdr windows) (list (car windows)))))
    (mapcar* (lambda (w b) (set-window-buffer w b))
             windows buffers)
    (other-window 1)))

(defun find-function-at-point-same-window ()
  "Finds directly the function at point in the current window."
  (interactive)
  (let ((symb (function-called-at-point))
        (find-function-recenter-line nil))
    (when symb (find-function symb))))

(defmacro defadvice* (name type functions &rest body)
  `(mapc (lambda (func)
           (ad-add-advice
            func '(,name nil t (advice . (lambda () ,@body)))
            ',type 'last)
           (ad-activate func nil))
         ',functions))

(defun align-to-equals (begin end)
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

(defmacro add-lambda (hook &rest body)
  (declare (indent 1))
  `(add-hook ,hook (lambda () ,@body)))

(provide 'defuns)
