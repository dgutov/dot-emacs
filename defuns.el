(defmacro setq-local (var value)
  "Makes VAR buffer-local and sets its value."
  `(set (make-local-variable ',var) ,value))

(defun xor (a b)
  (if a (not b) b))

(defvar autoload-directory-list `(,user-emacs-directory)
  "List of directories to generate autoloads from.")

(defun update-autoloads (&optional force-regen)
  (interactive "p")
  (let ((generated-autoload-file autoload-file)
        (dirs (if (or force-regen (not (file-exists-p autoload-file)))
                  autoload-directory-list
                (loop for directory in autoload-directory-list
                      when (some (lambda (f) (file-newer-than-file-p f autoload-file))
                                 (directory-files directory t "\\.el$"))
                      collect directory))))
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
        (call-remapped 'kill-word arg))
    (whack-whitespace nil)))

(defun backward-kill-word-dwim (arg)
  (interactive "p")
  (when (zerop (whack-whitespace t t))
    (if (zerop (whack-nonchars t))
        (call-remapped 'backward-kill-word arg))))

(defun call-remapped (command &rest arguments)
  (let ((new (command-remapping command)))
    (apply (or new command) arguments)))

(defun kill-region-dwim (arg)
  (interactive "p")
  (cond ((use-region-p)
         (kill-region (point) (mark)))
        (cua--rectangle
         (cua-cut-rectangle arg))
        (t
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
  (let ((dir (dired-current-directory)))
    (find-alternate-file "..")
    (dired-goto-file dir)))

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

(defadvice yas/parse-template (before clear-newlines () activate)
  (goto-char (point-max))
  (skip-chars-backward "\n\r\t ")
  (delete-region (point) (point-max)))

(defun rotate-windows ()
  (interactive)
  (let* ((windows (window-list))
         (buffers (mapcar 'window-buffer windows))
         (windows (nconc (cdr windows) (list (car windows)))))
    (mapcar* (lambda (w b) (set-window-buffer w b))
             windows buffers)
    (other-window 1)))

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

(defvar webjump-api-sites nil)

(make-variable-buffer-local 'webjump-api-sites)

(defun webjump-api ()
  (interactive)
  (require 'webjump)
  (let* ((completion-ignore-case t)
         (default (caar webjump-api-sites))
         (url (cdr (assoc-string
                    (completing-read "Search API: " webjump-api-sites nil t
                                     nil nil default)
                    webjump-api-sites t)))
         (name (completing-read "Name: " nil nil nil (thing-at-point 'symbol))))
    (browse-url (if (webjump-null-or-blank-string-p name)
                    url
                  (concat url (webjump-url-encode name))))))

;; http://ru-emacs.livejournal.com/82428.html
(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(defadvice* ignore-reverse-input-method around (read-passwd quoted-insert)
  (let ((local-function-key-map (make-keymap)))
    ad-do-it))

(provide 'defuns)
