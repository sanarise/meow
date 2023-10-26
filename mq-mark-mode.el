;;; mq-mark-mode.el --- Mq-mark mode  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Mq-mark mode.

;;; Code:

(defun mq-meow-insert ()
  "Switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--cancel-selection)
    (meow--switch-state 'insert)))

(defun mq-meow-beacon-insert ()
  "Insert and start kmacro recording."
  (interactive)
  (meow-beacon-mode -1)
  (mq-meow-insert)
  (call-interactively #'kmacro-start-macro)
  (setq-local meow--beacon-insert-enter-key last-input-event)
  (setq meow--beacon-defining-kbd-macro 'quick))

(define-key meow-beacon-state-keymap [remap mq-meow-insert] 'mq-meow-beacon-insert)

(defun mq-mark-line (&optional arg allow-extend)
  (interactive "P\np")
  (cond ((and allow-extend
	            (or (and (eq last-command this-command) (mark t))
		              (region-active-p)))
	       (setq arg (if arg (prefix-numeric-value arg)
		                 (if (< (mark) (point)) 1 -1)))
         (forward-line arg))
	      (t
         (beginning-of-line)
	       (push-mark nil t t)
	       (forward-line (prefix-numeric-value arg)))))

(defun mq-mark-whole-paragraph (&optional arg allow-extend)
  (interactive "P\np")
  (cond ((and allow-extend
	            (or (and (eq last-command this-command) (mark t))
                  (region-active-p)))
		     ;; (and transient-mark-mode mark-active)))
	       (setq arg (if arg (prefix-numeric-value arg)
		                 (if (< (mark) (point)) 1 -1)))
	       (mq-forward-paragraph arg))
	      (t
         (mq-backward-paragraph)
         (push-mark nil t t)
	       (mq-forward-paragraph (prefix-numeric-value arg)))))

(defun mq-to-indentation ()
  "Back to indentation, then meow-join."
  (interactive)
  (let ((pt (point)))
    (meow-back-to-indentation)
    ;; Есть проблема с тем, что сбрасывает выделение, если оно уже есть.
    ;; Но фикс ниже ломает логику выделения многострочного разрыва, если стоять на пустой строке.
    ;; (when (and (not mark-active) (eq pt (point)))
    (when (eq pt (point))
      (command-execute 'meow-join))))

(defun mq-to-eol ()
  (interactive)
  (let ((pt (point)))
    (end-of-line)
    (when (eq pt (point))
      (beginning-of-line))))

(defun mq-open-below ()
  (interactive)
  (if mark-active
      (command-execute 'meow-reverse)
    (command-execute 'meow-open-below)))

(defun mq-forward-word (&optional arg)
  (interactive "^p")
  (setq arg (or arg 1))
  (let ((word-bounds (bounds-of-thing-at-point 'word)))
    (forward-word
     (if (or (not word-bounds) (>= (point) (cdr word-bounds)))
         arg
       (1+ arg))))
  (backward-word))

(defun mq-forward-whitespace (arg)
  (interactive "^p")
  (if (natnump arg)
      (re-search-forward "\\s *\n\\{1\\}\\s *\\|\\s +" nil 'move arg)
    (while (< arg 0)
      (if (re-search-backward "\\(\\s +\\|^\\)\\S " nil 'move)
	        (skip-syntax-forward "-"))
      (setq arg (1+ arg)))))

(defun mq-backward-whitespace (arg)
  (interactive "p")
  (mq-forward-whitespace (- arg)))

(defun mq-forward-paragraph (arg)
  (interactive "^p")
  (if (natnump arg)
      (re-search-forward "\\s *\n\\s *\n" nil 'move arg)
    (while (< arg 0)
      (if (re-search-backward "\\s *\n\\s *\n\\s *\\S " nil 'move)
          (forward-line 2))
      (setq arg (1+ arg)))))

(defun mq-backward-paragraph (arg)
  (interactive "p")
  (mq-forward-paragraph (- arg)))

(defun mq-forward-to-symbol (arg)
  (interactive "^p")
  (or (re-search-forward (if (> arg 0) "\\W\\_<" "\\_<") nil t arg)
      (goto-char (if (> arg 0) (point-max) (point-min)))))

(defun mq-backward-to-symbol (arg)
  (interactive "^p")
  (mq-forward-to-symbol (- (or arg 1))))

(defun mq-mark-by-command (command &optional arg allow-extend)
  (interactive "P\np")
  (cond ((and allow-extend
	            (or (and (eq last-command this-command) (mark t))
		              (region-active-p)))
	       (setq arg (if arg (prefix-numeric-value arg)
		                 (if (< (mark) (point)) -1 1)))
	       (set-mark
          (save-excursion
	          (goto-char (mark))
            (funcall-interactively command arg)
	          (point))))
	      (t
	       (push-mark
	        (save-excursion
            (funcall-interactively command (prefix-numeric-value arg))
	          (point))
	        nil t))))

(defun mq-mark-to-symbol (&optional arg allow-extend)
  (interactive "P\np")
  (funcall-interactively #'mq-mark-by-command #'mq-forward-to-symbol arg allow-extend))

(defun mq-mark-backward-word (&optional arg allow-extend)
  (interactive "P\np")
  (funcall-interactively #'mq-mark-by-command #'mq-prev-word arg allow-extend))

(defun mq-mark-to-word (&optional arg allow-extend)
  (interactive "P\np")
  (funcall-interactively #'mq-mark-by-command #'mq-next-word arg allow-extend))

(defun mq-mark-forward-paragraph (&optional arg allow-extend)
  (interactive "P\np")
  (funcall-interactively #'mq-mark-by-command #'mq-forward-paragraph arg allow-extend))

(defun mq-mark-backward-paragraph (&optional arg allow-extend)
  (interactive "P\np")
  (funcall-interactively #'mq-mark-by-command #'mq-backward-paragraph arg allow-extend))

(defun mq-yank-above ()
  (interactive)
  (goto-char (line-beginning-position))
  (save-mark-and-excursion
    (newline))
  (indent-according-to-mode)
  (yank))

(defun mq-move-region (beg end arg)
  (let ((region (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (delete-char 1)
    (forward-line (- arg 1))
    (goto-char (line-end-position))
    (newline)
    (insert region)))

(defun mq-move-line (arg)
  (if (<= (+ (line-number-at-pos) arg) (count-lines (point-min) (point-max)))
      (let ((column (current-column))
            (beg (line-beginning-position))
            (end (line-end-position)))
        (mq-move-region beg end arg)
        (move-to-column column))
    (message "Can not move line further down")))

(defun mq-move-line-down (arg)
  (interactive "p")
  (mq-move-line arg))

(defun mq-move-line-up (arg)
  (interactive "p")
  (mq-move-line (- arg)))

(provide 'mq-mark-mode)
;;; mq-mark-mode.el ends here
