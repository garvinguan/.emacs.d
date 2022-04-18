;;;;;;;;;;;;;; Load Files ;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jump to todoes file
(defun load-todos-file ()
  "Edit the ~/org/todos.org in window."
  (interactive)
  (find-file "~/org/todos.org"))

;; Jump to notes file
(defun load-notes-file ()
  "Edit the ~/org/notes.org in window."
  (interactive)
  (find-file "~/org/notes.org"))

;; Jump to Init File
(defun init-file ()
  "Edit the `user-init-file', in window."
  (interactive)
  (find-file user-init-file))

(defun my-split-window-vertical ()
  "Vertical Split window with another buffer."
  (interactive)
  (select-window (split-window-right))
  (switch-to-buffer (other-buffer)))

(defun my-split-window-horizontal ()
  "Vertical Split window with another buffer."
  (interactive)
  (select-window (split-window-below))
  (switch-to-buffer (other-buffer)))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))


;; Save All Func
(defun save-all ()
  (interactive)
  (save-some-buffers t))

(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))
