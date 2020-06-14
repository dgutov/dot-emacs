;;; pop-up-mini.el --- pop up mini window in child frame -*- lexical-binding:t -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Martin Rudalics <rudalics@gmx.at>
;; Keywords: frames, minibuffers, windows
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))

;; pop-up-mini.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; pop-up-mini.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode to pop up minibuffer and echo area in a child frame of
;; the selected frame.  This mode automatizes the following behaviors:
;;
;; - Reparent child frame when switching frames.
;;
;; - Facultatively host child frame at the bottom or top of selected
;;   window or selected frame's main or root window.
;;
;; - Resize and reposition child frame corresponding to size of its
;;   text and/or host window.
;;
;; - Hide empty child frame.
;;
;; A sample form with recommended variable settings for customizing
;; the .emacs init file is included at the end of this file.

;; Note: This file will work only with Emacs 27.1 and higher.

;;; Code:

(defvar pop-up-mini-mode)

(defvar pop-up-mini-frame nil
  "The minibuffer child-frame of 'pop-up-mini-mode'.
Usually displayed as child frame of the selected frame.")

(defvar pop-up-mini-window nil
  "The root window of 'pop-up-mini-frame'.")

(defgroup pop-up-mini nil
  "Customization group for 'pop-up-mini-mode'."
  :version "27.1"
  :group 'convenience
  :group 'frames)

(defun pop-up-mini--set-value (symbol value)
  "Helper function for customizing 'pop-up-mini-mode' options."
  (set-default symbol value)
  (cond
   ((not (frame-live-p pop-up-mini-frame)))
   ((eq symbol 'pop-up-mini-internal-border)
    (set-face-background 'internal-border value pop-up-mini-frame))
   ((frame-visible-p pop-up-mini-frame)
    (when (frame-live-p (frame-parent pop-up-mini-frame))
      (pop-up-mini-move (frame-parent pop-up-mini-frame))))))

(defcustom pop-up-mini-host 'selected
  "Type of window hosting 'pop-up-mini-frame'.
Choices are

'selected' - the selected window

'main' - the selected frame's main window

'root' - the selected frame's root window

Note that both 'main' and 'root' may overwrite the mode lines of
the windows at the bottom of the frame."
  :type '(choice (const selected)
                 (const main)
                 (const root))
  :initialize 'custom-initialize-default
  :set 'pop-up-mini--set-value
  :version "27.1"
  :group 'pop-up-mini)

(defcustom pop-up-mini-host-min-width 20
  "Minimum width of host window for 'pop-up-mini-frame'.
If the window designated by 'pop-up-mini-host' is not as wide as
specified by this option, rehost 'pop-up-mini-frame' at the main
or root window of its parent frame."
  :type 'integer
  :initialize 'custom-initialize-default
  :set 'pop-up-mini--set-value
  :version "27.1"
  :group 'pop-up-mini)

(defcustom pop-up-mini-host-min-height 3
  "Minimum height of of host window for 'pop-up-mini-frame'.
If the window designated by 'pop-up-mini-host' is not as high as
specified by this option, rehost 'pop-up-mini-frame' at the main
or root window of its parent frame."
  :type 'integer
  :initialize 'custom-initialize-default
  :set 'pop-up-mini--set-value
  :version "27.1"
  :group 'pop-up-mini)

(defcustom pop-up-mini-rehost-to-fit t
  "If non-nil, rehost 'pop-up-mini-frame' when its text does not fit.
If this option is non-nil, 'pop-up-mini-host' equals 'selected'
and the text shown in 'pop-up-mini-frame' exceeds the width of
the selected window, rehost 'pop-up-mini-frame' to the selected
frame's main window."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'pop-up-mini--set-value
  :version "27.1"
  :group 'pop-up-mini)

(defcustom pop-up-mini-hide-if-empty 0.5
  "If non-nil, delay for hiding the empty 'pop-up-mini-frame'.
If this is nil, never hide the 'pop-up-mini-frame'.  Otherwise,
it specifies the number of seconds to wait before hiding an empty
'pop-up-mini-frame'.  Zero means to hide 'pop-up-mini-frame'
immediately after it becomes empty."
  :type '(choice (const nil)
		 (number :tag "delay"))
  :initialize 'custom-initialize-default
  :set 'pop-up-mini--set-value
  :version "27.1"
  :group 'pop-up-mini)

(defcustom pop-up-mini-position 'scroll-bar
  "Position of 'pop-up-mini-frame' within its host window.
Choices are

'body-bottom' - at the bottom of the host's body if the host is a
   live window, at the bottom of the host window otherwise.  For
   a live host window this avoids covering the window's mode line
   and any scroll bars, fringes, or margins.

'body-top' - at the top of the host window's body if the host is
   a live window, at the top of the host window otherwise.  For a
   live host window this avoids covering the window's header line
   and any scroll bars, fringes, or margins.

'scroll-bar' - above the mode line of the host window if it is
   live, at the bottom of the host window otherwise.  This will
   usually hide the horizontal scroll bar of the host window.

'bottom' - at the bottom of the host window.  This will usually
   cover the host window's mode line, if any.

'top' - at the top of the host window.  This will usually cover
   the host window's header line, if any.

The width of 'pop-up-mini-frame' is the body width of the host
window for the first two options and the total width of the host
window for the remaining ones."
  :type '(choice (const body-bottom)
                 (const body-top)
		 (const scroll-bar)
                 (const bottom)
                 (const top))
  :initialize 'custom-initialize-default
  :set 'pop-up-mini--set-value
  :version "27.1"
  :group 'pop-up-mini)

(defcustom pop-up-mini-resize-manually t
  "If non-nil, allow manually resizing of 'pop-up-mini-frame'.
This allows to drag the upper border of the 'pop-up-mini-frame'
with the mouse provided its window is the active minibuffer
window, until it either shrinks back to one line or becomes
empty."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set 'pop-up-mini--set-value
  :version "27.1"
  :group 'pop-up-mini)

(defcustom pop-up-mini-internal-border "blue"
  "Background of internal border of 'pop-up-mini-frame'."
  :type 'string
  :set 'pop-up-mini--set-value
  :version "27.1"
  :group 'pop-up-mini)

(defvar pop-up-mini-buffer nil
  "Present or past buffer of 'pop-up-mini-window'.
This is the buffer shown in 'pop-up-mini-window' the last time
'resize-mini-frame' was called for its frame.")

(defvar pop-up-mini-text ""
  "Text displayed in 'pop-up-mini-window'.")

(defvar pop-up-mini-text-pixel-size '(0 . 0)
  "Size of 'pop-up-mini-text' the last time it was calculated.
The return value of 'window-text-pixel-size' the last time it was
run for 'pop-up-mini-window'.  The X-LIMIT for that run was the
width of the parent frame of 'pop-up-mini-frame' at that time.
The buffer text of 'pop-up-mini-window' at that time was
'pop-up-mini-text'.")

(defvar pop-up-mini-window-pixel-width 0
  "Pixel width of 'pop-up-mini-window'.")

(defvar pop-up-mini-window-pixel-height 0
  "Pixel height of 'pop-up-mini-window'.")

(defvar pop-up-mini-block-height nil
  "Non-nil means temporarily block height adjustment.")

(defvar pop-up-mini-hide-if-empty-timer nil
  "Timer for hiding 'pop-up-mini-frame'.")

(defun pop-up-mini-host-window (&optional frame)
  "Return window to host 'pop-up-mini-frame' on FRAME.
FRAME defaults to the selected frame."
  (cond
   ((eq pop-up-mini-host 'main)
    (window-main-window frame))
   ((eq pop-up-mini-host 'root)
    (frame-root-window frame))
   (t
    (frame-selected-window frame))))

(defun pop-up-mini-hide ()
  "Hide pop-up-mini-frame."
  (when (frame-live-p pop-up-mini-frame)
    (make-frame-invisible pop-up-mini-frame)
    (when (timerp pop-up-mini-hide-if-empty-timer)
      (cancel-timer pop-up-mini-hide-if-empty-timer)
      (setq pop-up-mini-hide-if-empty-timer nil))))

(defun pop-up-mini-move (frame &optional hide-immediately)
  "Maybe move and resize 'pop-up-mini-frame' to FRAME."
  (let* ((host (pop-up-mini-host-window frame))
	 (position pop-up-mini-position)
	 (body (and (window-live-p host)
		    (memq position '(body-bottom body-top))))
	 (edges (window-edges host body nil t))
	 (borders-width
	  (* (frame-parameter pop-up-mini-frame 'internal-border-width) 2))
	 (body-width (car pop-up-mini-text-pixel-size))
	 (body-height (cdr pop-up-mini-text-pixel-size))
	 total-width total-height extended-width) ; extended-main)

;;     (foo-it hide-immediately)

    (if (and (numberp pop-up-mini-hide-if-empty)
	     (zerop body-width) (zerop body-height))
	(cond
	 ((or hide-immediately (<= pop-up-mini-hide-if-empty 0))
	  ;; An empty 'pop-up-mini-buffer' - hide it.
	  (setq pop-up-mini-block-height nil)
	  (make-frame-invisible pop-up-mini-frame))
	 (t
	  (setq pop-up-mini-block-height nil)
	  (setq pop-up-mini-hide-if-empty-timer
		(run-with-timer
		 pop-up-mini-hide-if-empty nil 'pop-up-mini-hide))))
      ;; If 'pop-up-mini-resize-manually' is non-nil, preserve height
      ;; of a manually resized 'pop-up-mini-frame' as long as its
      ;; window is the active minibuffer window, is larger than one
      ;; line and its buffer is non-empty.
      (if pop-up-mini-block-height
	  (setq pop-up-mini-block-height
		(and (minibuffer-window-active-p pop-up-mini-window)
		     (not (zerop body-width))
		     (not (zerop body-height))
		     (> (window-pixel-height pop-up-mini-window)
			(frame-char-height pop-up-mini-frame))))
	(setq pop-up-mini-block-height
	      (and pop-up-mini-resize-manually
		   (minibuffer-window-active-p pop-up-mini-window)
		   (/= (window-body-height pop-up-mini-window t)
		       body-height))))

;;       (foo-it pop-up-mini-hide-if-empty body-width body-height)

      ;; Check minimum width and height of selected window.
      (cond
       ;; For 'scroll-bar' detract mode line and divider height from
       ;; third edge.
       ((and (eq position 'scroll-bar) (eq pop-up-mini-host 'selected))
	(setq edges (list (nth 0 edges) (nth 1 edges) (nth 2 edges)
			  (- (nth 3 edges) (window-mode-line-height host)
			     (window-bottom-divider-width host))))
	(setq position 'bottom))
       ;; For 'bottom' detract divider height from third edge when host
       ;; is live ('main' slightly loses here).
       ((and (eq position 'bottom) (window-live-p host))
	(setq edges (list (nth 0 edges) (nth 1 edges) (nth 2 edges)
			  (- (nth 3 edges)
			     (window-bottom-divider-width host))))))
      (setq total-width
	    (+ body-width
	       (- (window-pixel-width pop-up-mini-window)
		  (window-body-width pop-up-mini-window t))
	       (- (window-scroll-bar-width pop-up-mini-window))
	       (- (let ((fringes (window-fringes pop-up-mini-window)))
		    (+ (car fringes) (nth 1 fringes))))))

      (unless pop-up-mini-block-height
	(setq total-height
	      (+ body-height
		 (let ((scroll-bars (window-scroll-bars pop-up-mini-window)))
		   (or (nth 3 scroll-bars)
		       (and (eq (nth 5 scroll-bars) t)
			    (frame-scroll-bar-height pop-up-mini-frame))
		       0))
		 (- (frame-scroll-bar-height pop-up-mini-frame)))))

      ;; Extend on scroll bars, maybe.
      (when (and (eq pop-up-mini-position 'scroll-bar)
		 (window-combined-p nil t)
		 (or (< (- (nth 2 edges) (nth 0 edges)
			   (window-right-divider-width host))
			total-width)
		     (< (- (nth 2 edges) (nth 0 edges)
			   (window-right-divider-width host))
			pop-up-mini-host-min-width)))
	(let* ((edge-0 (nth 0 edges))
	       (sibling (window-next-sibling))
	       (edge-2 (nth 2 (window-edges sibling nil nil t)))
	       (width (- edge-2 edge-0)))
	  (while (and sibling (< width total-width))
	    (setq sibling (window-next-sibling sibling))
	    (setq edge-2 (nth 2 (window-edges sibling nil nil t)))
	    (setq width (- edge-2 edge-0)))
	  (if (>= width total-width)
	      (progn
		(setq extended-width width)
;; 		(setq extended-main
;; 		      (and sibling (not (window-live-p sibling))))
		)
	    (setq edge-2 (nth 2 (window-edges
				 (window-main-window frame) nil nil t)))
	    (setq width (- edge-2 edge-0))
	    (when (>= width total-width)
;; 	      (setq extended-main t)
	      (setq extended-width width)))))

      (when (and (not pop-up-mini-block-height)
		 (eq pop-up-mini-host 'selected)
		 (not extended-width)
		 (or (not (zerop pop-up-mini-host-min-width))
		     (not (zerop pop-up-mini-host-min-height))
		     pop-up-mini-rehost-to-fit)
		 (or (and (/= (window-total-width host)
			      (window-total-width (window-main-window frame)))
			  (or (< (- (nth 2 edges) (nth 0 edges)
				    (window-right-divider-width host))
				 total-width)
			      (< (- (nth 2 edges) (nth 0 edges)
				    (window-right-divider-width host))
				 pop-up-mini-host-min-width)
		     (and (/= (window-total-height host)
			      (window-total-height (window-main-window frame)))
			  (or (< (- (nth 3 edges) (nth 1 edges)) total-height)
			      (< (- (nth 3 edges) (nth 1 edges))
				 pop-up-mini-host-min-height)))))))
	;; FRAME's selected window is too small, use its main window.
	(setq host (window-main-window frame))
	(setq position
	      (cond
	       ((eq position 'body-top) 'top)
	       ((eq position 'body-bottom) 'bottom)
	       (t position)))
	(setq edges (window-edges host nil nil t)))

;;       (foo-it pop-up-mini-buffer pop-up-mini-text
;; 	      pop-up-mini-text-pixel-size
;; 	      host (- (nth 2 edges) (nth 0 edges)) body-width)

;;       (foo-it (- (frame-pixel-height frame) (nth 3 edges))
;; 	      total-height)

      ;; Cancel our timer.
      (when (timerp pop-up-mini-hide-if-empty-timer)
	(cancel-timer pop-up-mini-hide-if-empty-timer)
	(setq pop-up-mini-hide-if-empty-timer nil))
      ;; Resize and position.
      (modify-frame-parameters
       pop-up-mini-frame
       `((parent-frame . ,frame)
	 (visibility . t)
	 (top . ,(if (memq pop-up-mini-position '(top body-top))
		     (nth 1 edges)
		   `(- ,(- (frame-pixel-height frame) (nth 3 edges)))))
	 (left . ,(nth 0 edges))
	 ,(unless pop-up-mini-block-height
	    `(height . ,(cons 'text-pixels
			      ;; Don't make 'pop-up-mini-frame' higher
			      ;; than its host window.
			      (min (- (nth 3 edges) (nth 1 edges))
				   total-height))))
	 (width . ,(cons 'text-pixels
			 ;; Make 'pop-up-mini-frame' as wide as its
			 ;; host window.
			 (or (and extended-width
				  (- extended-width
				     (- (window-pixel-width pop-up-mini-window)
					(window-body-width pop-up-mini-window t))
				     borders-width))
			     (- (nth 2 edges) (nth 0 edges)
				(if (window-live-p host)
				    (window-right-divider-width host)
				  0)
				(- (window-pixel-width pop-up-mini-window)
				   (window-body-width pop-up-mini-window t))
				borders-width))))))
      (setq pop-up-mini-window-pixel-width
	    (window-pixel-width pop-up-mini-window))
      (setq pop-up-mini-window-pixel-height
	    (window-pixel-height pop-up-mini-window)))))

(defvar pop-up-mini-window-reparented nil)

(defun pop-up-mini-window-state-change ()
  "'pop-up-mini-mode' function run by 'window-state-change-hook'.
If the selected window, the window specfied by 'pop-up-mini-host'
or 'pop-up-mini-text' changed since last redisplay, reparent,
resize and/or reposition 'pop-up-mini-frame' if necessary."
  (when (frame-live-p pop-up-mini-frame)
    (setq pop-up-mini-window-reparented
	  (and pop-up-mini-window-reparented
	       (not (zerop (minibuffer-depth)))))

    (let* ((old-parent
	    (and (frame-live-p (frame-parent pop-up-mini-frame))
		 (frame-parent pop-up-mini-frame)))
	   (new-parent
	    (cond
	     ((eq (selected-frame) old-parent) old-parent)
	     ((eq (selected-frame) pop-up-mini-frame)
	      (if (minibuffer-selected-window)
		  (let ((frame (window-frame (minibuffer-selected-window))))
		    (if (and (not pop-up-mini-window-reparented)
			     (not (eq frame old-parent))
			     (eq (minibuffer-window frame) pop-up-mini-window))
			;; Special case where selected window changed
			;; _and_ 'pop-up-mini-window' got selected.
			(progn
			  (setq pop-up-mini-window-reparented t)
			  frame)
		      old-parent))))
	     ((eq (minibuffer-window) pop-up-mini-window) (selected-frame))
	     (t old-parent)))
	   hide-immediately)

;;       (unless (eq (frame-old-selected-window new-parent)
;; 		  (frame-selected-window new-parent))
;; 	(foo-it (frame-old-selected-window new-parent)
;; 		(frame-selected-window new-parent)))

      (when (and (frame-live-p new-parent)
		 (or (setq hide-immediately (not (eq old-parent new-parent)))
		     (let ((host-window (pop-up-mini-host-window new-parent)))
		       (setq hide-immediately
			     (or (and (eq pop-up-mini-host 'selected)
				      (not (eq (frame-old-selected-window
						new-parent)
					       (frame-selected-window
						new-parent))))
				 (/= (window-pixel-height host-window)
				     (window-old-pixel-height host-window))
				 (/= (window-pixel-width host-window)
				     (window-old-pixel-width host-window)))))
		     (frame-window-state-change pop-up-mini-frame)))
	;; Something really changed, dig further.
	(pop-up-mini-move new-parent hide-immediately)))))

(defun pop-up-mini-resize-mini-frame (frame)
  "'resize-mini-frames' function of 'pop-up-mini-mode'."
  (when (eq frame pop-up-mini-frame)
    (let* ((buffer (window-buffer pop-up-mini-window))
	   (text (with-current-buffer buffer
		   ;; Since we ignore text properties we will miss the
		   ;; case of displaying the same string with
		   ;; different text properties twice in a row.
		   (buffer-substring-no-properties (point-min) (point-max)))))
      ;; Return when there's no change.  This means that
      ;; 'pop-up-mini-buffer', 'pop-up-mini-text' as well as width and
      ;; height of 'pop-up-mini-window' are the same as the last time
      ;; we displayed them.
      (unless (and (eq buffer pop-up-mini-buffer)
		   (string-equal pop-up-mini-text text)
		   ;; Make sure that we do not miss a case where we
		   ;; want to make 'pop-up-mini-frame' invisible.
		   (or (not pop-up-mini-hide-if-empty)
		       (not (string-equal text ""))
		       (not (frame-visible-p pop-up-mini-frame)))
		   (equal (window-pixel-width pop-up-mini-window)
			  pop-up-mini-window-pixel-width)
		   (equal (window-pixel-height pop-up-mini-window)
			  pop-up-mini-window-pixel-height))
	;; Remember old string.
	(setq pop-up-mini-buffer buffer)
	(setq pop-up-mini-text text)
	(setq pop-up-mini-window-pixel-width
	      (window-pixel-width pop-up-mini-window))
	(setq pop-up-mini-window-pixel-height
	      (window-pixel-height pop-up-mini-window))
	;; This is the only place where we can calculate the size.
	(setq pop-up-mini-text-pixel-size
	      (and (buffer-live-p buffer)
		   (window-text-pixel-size
		    pop-up-mini-window
		    nil nil (frame-text-width
			     (frame-parent pop-up-mini-frame)))))

;; 	(when (buffer-live-p buffer)
;; 	  (foo-it buffer (eq buffer pop-up-mini-buffer)
;; 		  text (string-equal text pop-up-mini-text)
;; 		  pop-up-mini-text-pixel-size
;; 		  (frame-text-width
;; 		   (frame-parent pop-up-mini-frame))))

	;; Notify ourselves of change.
	(set-frame-window-state-change frame t)))))

(defun pop-up-mini-after-make-frame (frame)
  "Maybe make FRAME client of 'pop-up-mini-frame'."
  (when (and (not (frame-parameter frame 'minibuffer))
	     (window-live-p pop-up-mini-window))
    (set-frame-parameter frame 'minibuffer pop-up-mini-window)))

(defun pop-up-mini-setup ()
  "Set up minibuffer child frame mode.
This function makes the first minibuffer-only frame it finds the
'pop-up-mini-frame'.  Needs a minibuffer-only frame."
  (setq pop-up-mini-frame nil)

  ;; Make 'pop-up-mini-frame' the first 'minibuffer-only' frame found
  ;; on 'frame-list'.  Throw an error if we don't find one.
  (unless (catch 'found
	    (dolist (frame (frame-list))
	      (when (eq (frame-parameter frame 'minibuffer) 'only)
		(throw 'found (setq pop-up-mini-frame frame)))))
    (error "'pop-up-mini-mode' needs a minibuffer-only frame"))

  ;; The following will fail whenever someone resets the
  ;; 'unsplittable' parameter of 'pop-up-mini-frame'.
  (setq pop-up-mini-window (frame-root-window pop-up-mini-frame))
  (setq pop-up-mini-buffer (window-buffer pop-up-mini-window))
  (set-face-background
   'internal-border pop-up-mini-internal-border pop-up-mini-frame)
  ;; Usurpate 'resize-mini-frames'.  On exit reset that to nil.
  (setq resize-mini-frames 'pop-up-mini-resize-mini-frame)
  (add-hook
   'window-state-change-hook 'pop-up-mini-window-state-change)
  (add-hook
   'after-make-frame-functions 'pop-up-mini-after-make-frame))

;;;###autoload
(define-minor-mode pop-up-mini-mode
  "Show minibuffer and echo area in a child frame.
'pop-up-mini-mode' uses a child frame called 'pop-up-mini-frame'
for displaying the minibuffer or the echo area.  The parent frame
of 'pop-up-mini-frame' is the currently selected frame.  Within
its parent, 'pop-up-mini-frame' can be hosted at that frame's
selected, main or root window (see 'pop-up-mini-host') either at
that window's top or bottom (see 'pop-up-mini-position').  A
'pop-up-mini-frame' that does not display any text can be hidden
using the option 'pop-up-mini-hide-if-empty'."
  :global t
  :group 'pop-up-mini
  :init-value nil
  :link '(emacs-commentary-link "pop-up-mini.el")
  (if pop-up-mini-mode
      (if frame-initial-frame
	  ;; When the initial frame is still around wait until
	  ;; 'window-setup-hook' is run.
	  (add-hook 'window-setup-hook 'pop-up-mini-setup)
	;; Otherwise run immediately.
	(pop-up-mini-setup))
    ;; Cancel our timer.
    (when (timerp pop-up-mini-hide-if-empty-timer)
      (cancel-timer pop-up-mini-hide-if-empty-timer)
      (setq pop-up-mini-hide-if-empty-timer nil))
    (remove-hook
     'window-state-change-hook 'pop-up-mini-window-state-change)
    (remove-hook
     'after-make-frame-functions 'pop-up-mini-after-make-frame)
    ;; This is not very nice but any previous value might be broken.
    (setq resize-mini-frames nil)
    ;; Reset our variables.
    (setq pop-up-mini-window nil)
    (when pop-up-mini-frame
      ;; Unparent our frame and make it visible.
      (modify-frame-parameters
       pop-up-mini-frame
       `((parent-frame . nil) (visibility . t)))
      (setq pop-up-mini-frame nil))))

(provide 'pop-up-mini)

;;; Recommended variable settings for .emacs init file.

(setq
 ;; Enable ourselves.
 ;pop-up-mini-mode 1
 ;; Resize frames exactly.
 frame-resize-pixelwise t
 ;; At least two lines scroll margin for selected window host.
 scroll-margin 2
 ;; Automatically create a minibuffer child frame at startup and
 ;; reparent it immediately.
 initial-frame-alist
 (cons '(minibuffer . child-frame) initial-frame-alist)
 ;; Reuse minibuffer child frame when making a new frame.  Do not use
 ;; '(minibuffer . child-frame) here, it would make a new child frame.
 default-frame-alist
 (cons '(minibuffer . nil) default-frame-alist)
 minibuffer-frame-alist
 '(;; Initial positioning and sizing for the window manager.
   (height . 1)
   (width . 1.0)
   (top . 1.0)
   ;; Always show at least one line.
   (min-height . 1)
   ;; No horizontal scroll bars (they don't work properly in
   ;; minibuffer windows).
   (horizontal-scroll-bars . nil)
   ;; No title bar.
   (undecorated . t)
   ;; To drag the child frame's top with the mouse.
   (internal-border-width . 1)
   (drag-internal-border . t)
   ;; No special glyphs.
   (no-special-glyphs . t)
   ;; No foucs when child frame is (re-)mapped.
   (no-focus-on-map . t)))

;;; pop-up-mini.el ends here

;; (set-frame-parameter (window-frame (minibuffer-window)) 'mini-window-horizontal-scroll-bar t)
;; (frame-parameter (window-frame (minibuffer-window)) 'mini-window-horizontal-scroll-bar)

;(pop-up-mini-mode 1)

(setq x-gtk-resize-child-frames 'resize-mode)
