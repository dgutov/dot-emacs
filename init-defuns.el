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

(defun conj (list element)
  "Returns LIST prepended by ELEMENT."
  (cons element list))

(defun update-hash (table key function &rest args)
  "Changes value associated with KEY to the one returned by FUNCTION.
The function is passed the old value and ARGS as arguments."
  (puthash key (apply function (gethash key table) args)
           table))

(defun hash-to-sorted-alist (table compare-fn)
  "Converts hash table to associative list sorted with supplied function."
  (let (alist)
    (maphash (lambda (k v) (add-to-list 'alist (cons k v))) table)
    (sort alist (lambda (e1 e2) (funcall compare-fn (car e1) (car e2))))))

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

(defun vc-dir (dir-name)
  (expand-file-name (concat "~/vc/" dir-name)))

(defun update-load-path-vc (dir-name &optional add-to-autoloads)
  (update-load-path (vc-dir dir-name) add-to-autoloads))

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

(defadvice nav (around reenable-emacs23-window-splitting () activate)
  "Blocks rebinding of window splitting vars."
  (let (split-width-threshold
        split-height-threshold)
    ad-do-it))

(dolist (func '(nav-mode nav-tags-mode nav-show-bufs))
  (ad-add-advice func '(window-size-fixed
                        nil t (advice . (lambda () (setq window-size-fixed t))))
                 'after 'last))

(dolist (func '(nav-shrink-wrap nav-set-width-to-default))
  (ad-add-advice func '(unfix-window-size
                        nil t (advice . (lambda () (let (window-size-fixed) ad-do-it))))
                 'around 'last))

(defadvice nav-show-bufs (before select-nav-window activate)
  (nav-select-nav-window))

(defalias 'nav-tags 'nav-tags-mode)

(defadvice nav-open-file (around current-window (filename) activate)
  "Opens file in an existing window."
  (if (file-directory-p filename)
      (nav-push-dir filename)
    (when (eq (selected-window) (next-window))
      (split-window-horizontally)
      (nav-set-width-to-default))
    (let ((filename (expand-file-name filename)))
      (other-window 1)
      (find-file filename))))

(defadvice nav-bufs-update (around keep-current-window () activate)
  (save-selected-window ad-do-it))

(defadvice nav-bufs-show-buffers (around group-buffers () activate)
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (group (nav-bufs-group-function (buffer-list)))
      (nav-insert-text (concat (car group) ":") nav-face-heading)
      (insert "\n")
      (dolist (bname (cdr group))
        (insert-text-button bname :type 'buffer-jump-button)
        (insert "\n"))
      (insert "\n"))
    (setq mode-line-format (nav-update-mode-line "b" default-directory))
    (force-mode-line-update))
  (setq truncate-lines t)
  (goto-line 2))

(defun nav-bufs-group-function (buffers)
  (let ((project-groups (make-hash-table :test 'equal))
        (major-mode-groups (make-hash-table :test 'equal))
        special-group)
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (let ((bname (buffer-name)))
          ;; skip temporary buffers, their names start with whitespace
          (unless (eq ?\s (aref bname 0))
            (cond ((string-match-p "^\\*.*\\*$" bname)
                   (add-to-list 'special-group bname))
                  (eproject-mode
                   (update-hash project-groups (eproject-name) #'conj bname))
                  (t
                   (update-hash major-mode-groups
                                (if (and (stringp mode-name)
                                         (string-match-p "[^ ]" mode-name))
                                    mode-name
                                  (symbol-name major-mode))
                                #'conj bname)))))))
    (mapc (lambda (pair) (setcdr pair (sort (cdr pair) 'string<))) ;; sort buffers by name inside each group
          `(,@(hash-to-sorted-alist project-groups 'string<)
            ,@(hash-to-sorted-alist major-mode-groups 'string<)
            ("Special" . ,special-group)))))

;;(defadvice nav-cd (before ping (dirname) activate)
;;  (message "nav-cd called!"))

(provide 'init-defuns)
