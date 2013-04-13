;;; vlfi.el --- View Large Files Improved  -*- lexical-binding: t -*-

;; Copyright (C) 2006, 2012, 2013  Free Software Foundation, Inc.

;; Version: 0.5
;; Keywords: large files, utilities
;; Authors: 2006 Mathias Dahl <mathias.dahl@gmail.com>
;;          2012 Sam Steingold <sds@gnu.org>
;;          2013 Andrey Kotlarski <m00naticus@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides the M-x vlfi command, which visits part of a
;; large file in a read-only buffer without visiting the entire file.
;; The buffer uses VLFI mode, which defines several commands for
;; moving around, searching and editing selected chunk of file.

;; This package is an improved fork of the vlf.el package.

;;; Code:

(defgroup vlfi nil
  "View Large Files in Emacs."
  :prefix "vlfi-"
  :group 'files)

(defcustom vlfi-batch-size 1024
  "Defines how large each batch of file data is (in bytes)."
  :type 'integer
  :group 'vlfi)

;;; Keep track of file position.
(defvar vlfi-start-pos 0
  "Absolute position of the visible chunk start.")
(defvar vlfi-end-pos vlfi-batch-size
  "Absolute position of the visible chunk end.")
(defvar vlfi-file-size 0 "Total size of presented file.")

(defvar vlfi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-next] 'vlfi-next-batch)
    (define-key map [M-prior] 'vlfi-prev-batch)
    (define-key map "+" 'vlfi-change-batch-size)
    (define-key map "-"
      (lambda () "Decrease vlfi batch size by factor of 2."
        (interactive)
        (vlfi-change-batch-size t)))
    (define-key map "s" 'vlfi-re-search-forward)
    (define-key map "r" 'vlfi-re-search-backward)
    (define-key map "[" 'vlfi-beginning-of-file)
    (define-key map "]" 'vlfi-end-of-file)
    (define-key map "e" 'vlfi-edit-mode)
    (define-key map "j" 'vlfi-jump-to-chunk)
    map)
  "Keymap for `vlfi-mode'.")

(define-derived-mode vlfi-mode special-mode "VLFI"
  "Mode to browse large files in."
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (buffer-disable-undo)
  (add-hook 'write-contents-functions 'vlfi-write)
  (setq revert-buffer-function 'vlfi-revert)
  (make-local-variable 'vlfi-batch-size)
  (put 'vlfi-batch-size 'permanent-local t)
  (make-local-variable 'vlfi-start-pos)
  (put 'vlfi-start-pos 'permanent-local t)
  (make-local-variable 'vlfi-end-pos)
  (put 'vlfi-end-pos 'permanent-local t)
  (make-local-variable 'vlfi-file-size)
  (put 'vlfi-file-size 'permanent-local t))

;;;###autoload
(defun vlfi (file &optional from-end)
  "View Large FILE.  With FROM-END prefix, view from the back.
Batches of the file data from FILE will be displayed in a read-only
buffer.  You can customize number of bytes displayed by customizing
`vlfi-batch-size'."
  (interactive "fFile to open: \nP")
  (with-current-buffer (generate-new-buffer "*vlfi*")
    (setq buffer-file-name file
          vlfi-file-size (nth 7 (file-attributes file)))
    (vlfi-insert-file from-end)
    (vlfi-mode)
    (switch-to-buffer (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; integration with other packages

;;;###autoload
(defun dired-vlfi (from-end)
  "In Dired, visit the file on this line in VLFI mode.
With FROM-END prefix, view from the back."
  (interactive "P")
  (vlfi (dired-get-file-for-visit) from-end))

;;;###autoload
(eval-after-load "dired"
  '(define-key dired-mode-map "V" 'dired-vlfi))

;;;###autoload
(defun vlfi-if-file-too-large (size op-type &optional filename)
  "If file SIZE larger than `large-file-warning-threshold', \
allow user to view file with `vlfi', open it normally or abort.
OP-TYPE specifies the file operation being performed over FILENAME."
  (and large-file-warning-threshold size
       (> size large-file-warning-threshold)
       (let ((char nil))
         (while (not (memq (setq char
                                 (read-event
                                  (propertize
                                   (format
                                    "File %s is large (%s): \
%s normally (o), %s with vlfi (v) or abort (a)"
                                    (if filename
                                        (file-name-nondirectory filename)
                                      "")
                                    (file-size-human-readable size)
                                    op-type op-type)
                                   'face 'minibuffer-prompt)))
                           '(?o ?O ?v ?V ?a ?A))))
         (cond ((memq char '(?o ?O)))
               ((memq char '(?v ?V))
                (vlfi filename nil)
                (error ""))
               ((memq char '(?a ?A))
                (error "Aborted"))))))

;; hijack `abort-if-file-too-large'
;;;###autoload
(fset 'abort-if-file-too-large 'vlfi-if-file-too-large)

;; non recent Emacs
(unless (fboundp 'file-size-human-readable)
  (defun file-size-human-readable (file-size)
    "Print FILE-SIZE in MB."
    (format "%.1fMB" (/ file-size 1024.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities

(defun vlfi-change-batch-size (decrease)
  "Change the buffer-local value of `vlfi-batch-size'.
Normally, the value is doubled;
with the prefix argument DECREASE it is halved."
  (interactive "P")
  (or (assq 'vlfi-batch-size (buffer-local-variables))
      (error "%s is not local in this buffer" 'vlfi-batch-size))
  (setq vlfi-batch-size (if decrease
                            (/ vlfi-batch-size 2)
                          (* vlfi-batch-size 2)))
  (vlfi-move-to-batch vlfi-start-pos))

(defun vlfi-format-buffer-name ()
  "Return format for vlfi buffer name."
  (format "%s(%s)[%d/%d](%d)"
          (file-name-nondirectory buffer-file-name)
          (file-size-human-readable vlfi-file-size)
          (/ vlfi-end-pos vlfi-batch-size)
          (/ vlfi-file-size vlfi-batch-size)
          vlfi-batch-size))

(defun vlfi-update-buffer-name ()
  "Update the current buffer name."
  (rename-buffer (vlfi-format-buffer-name) t))

(defun vlfi-insert-file (&optional from-end)
  "Insert first chunk of current file contents in current buffer.
With FROM-END prefix, start from the back."
  (if from-end
      (setq vlfi-start-pos (max 0 (- vlfi-file-size vlfi-batch-size))
            vlfi-end-pos vlfi-file-size)
    (setq vlfi-start-pos 0
          vlfi-end-pos (min vlfi-batch-size vlfi-file-size)))
  (vlfi-move-to-chunk vlfi-start-pos vlfi-end-pos))

(defun vlfi-beginning-of-file ()
  "Jump to beginning of file content."
  (interactive)
  (vlfi-insert-file))

(defun vlfi-end-of-file ()
  "Jump to end of file content."
  (interactive)
  (vlfi-insert-file t))

(defun vlfi-revert (&rest args)
  "Revert current chunk.  Ignore ARGS."
  (ignore args)
  (vlfi-move-to-chunk vlfi-start-pos vlfi-end-pos))

(defun vlfi-jump-to-chunk (n)
  "Go to to chunk N."
  (interactive "nGoto to chunk: ")
  (vlfi-move-to-batch (* (1- n) vlfi-batch-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; batch movement

(defun vlfi-next-batch (append)
  "Display the next batch of file data.
When prefix argument is supplied and positive
 jump over APPEND number of batches.
When prefix argument is negative
 append next APPEND number of batches to the existing buffer."
  (interactive "p")
  (let ((end (+ vlfi-end-pos (* vlfi-batch-size
                                (abs append)))))
    (when (< vlfi-file-size end)        ; re-check file size
      (setq vlfi-file-size (nth 7 (file-attributes buffer-file-name)))
      (cond ((= vlfi-end-pos vlfi-file-size)
             (error "Already at EOF"))
            ((< vlfi-file-size end)
             (setq end vlfi-file-size))))
    (let ((inhibit-read-only t)
          (do-append (< append 0))
          (pos (point)))
      (if do-append
          (goto-char (point-max))
        (setq vlfi-start-pos (- end vlfi-batch-size))
        (erase-buffer))
      (insert-file-contents buffer-file-name nil (if do-append
                                                     vlfi-end-pos
                                                   vlfi-start-pos)
                            end)
      (goto-char pos))
    (setq vlfi-end-pos end))
  (set-visited-file-modtime)
  (set-buffer-modified-p nil)
  (vlfi-update-buffer-name))

(defun vlfi-prev-batch (prepend)
  "Display the previous batch of file data.
When prefix argument is supplied and positive
 jump over PREPEND number of batches.
When prefix argument is negative
 append previous PREPEND number of batches to the existing buffer."
  (interactive "p")
  (if (zerop vlfi-start-pos)
      (error "Already at BOF"))
  (let ((inhibit-read-only t)
        (start (max 0 (- vlfi-start-pos (* vlfi-batch-size
                                           (abs prepend)))))
        (do-prepend (< prepend 0))
        (pos (- (point-max) (point))))
    (if do-prepend
        (goto-char (point-min))
      (setq vlfi-end-pos (+ start vlfi-batch-size))
      (erase-buffer))
    (insert-file-contents buffer-file-name nil start
                          (if do-prepend
                              vlfi-start-pos
                            vlfi-end-pos))
    (goto-char (- (point-max) pos))
    (setq vlfi-start-pos start))
  (set-visited-file-modtime)
  (set-buffer-modified-p nil)
  (vlfi-update-buffer-name))

(defun vlfi-move-to-batch (start)
  "Move to batch determined by START.
Adjust according to file start/end and show `vlfi-batch-size' bytes."
  (setq vlfi-start-pos (max 0 start)
        vlfi-end-pos (+ vlfi-start-pos vlfi-batch-size))
  (if (< vlfi-file-size vlfi-end-pos)   ; re-check file size
      (setq vlfi-file-size
            (nth 7 (file-attributes buffer-file-name))
            vlfi-end-pos (min vlfi-end-pos vlfi-file-size)
            vlfi-start-pos (max 0 (- vlfi-end-pos vlfi-batch-size))))
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (insert-file-contents buffer-file-name nil
                          vlfi-start-pos vlfi-end-pos)
    (goto-char pos))
  (set-visited-file-modtime)
  (set-buffer-modified-p nil)
  (vlfi-update-buffer-name))

(defun vlfi-move-to-chunk (start end)
  "Move to chunk determined by START END."
  (if (< vlfi-file-size end)            ; re-check file size
      (setq vlfi-file-size (nth 7
                                (file-attributes buffer-file-name))))
  (setq vlfi-start-pos (max 0 start)
        vlfi-end-pos (min end vlfi-file-size))
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (insert-file-contents buffer-file-name nil
                          vlfi-start-pos vlfi-end-pos)
    (goto-char pos))
  (set-visited-file-modtime)
  (set-buffer-modified-p nil)
  (vlfi-update-buffer-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; search

(defun vlfi-re-search (regexp count backward)
  "Search for REGEXP COUNT number of times forward or BACKWARD."
  (let* ((match-start-pos (+ vlfi-start-pos (point)))
         (match-end-pos match-start-pos)
         (to-find count)
         (search-reporter (make-progress-reporter
                           (concat "Searching for " regexp)
                           (if backward
                               (- vlfi-file-size vlfi-end-pos)
                             vlfi-start-pos)
                           vlfi-file-size))
         (batch-step (/ vlfi-batch-size 8))) ; amount of chunk overlap
    (unwind-protect
        (catch 'end-of-file
          (if backward
              (while (not (zerop to-find))
                (cond ((re-search-backward regexp nil t)
                       (setq to-find (1- to-find)
                             match-start-pos (+ vlfi-start-pos
                                                (match-beginning 0))
                             match-end-pos (+ vlfi-start-pos
                                              (match-end 0))))
                      ((zerop vlfi-start-pos)
                       (throw 'end-of-file nil))
                      (t (let ((batch-move (- vlfi-start-pos
                                              (- vlfi-batch-size
                                                 batch-step))))
                           (vlfi-move-to-batch
                            (if (< match-start-pos batch-move)
                                (- match-start-pos vlfi-batch-size)
                              batch-move)))
                         (goto-char (if (< match-start-pos
                                           vlfi-end-pos)
                                        (- match-start-pos
                                           vlfi-start-pos)
                                      (point-max)))
                         (progress-reporter-update search-reporter
                                                   vlfi-start-pos))))
            (while (not (zerop to-find))
              (cond ((re-search-forward regexp nil t)
                     (setq to-find (1- to-find)
                           match-start-pos (+ vlfi-start-pos
                                              (match-beginning 0))
                           match-end-pos (+ vlfi-start-pos
                                            (match-end 0))))
                    ((= vlfi-end-pos vlfi-file-size)
                     (throw 'end-of-file nil))
                    (t (let ((batch-move (- vlfi-end-pos batch-step)))
                         (vlfi-move-to-batch
                          (if (< batch-move match-end-pos)
                              match-end-pos
                            batch-move)))
                       (goto-char (if (< vlfi-start-pos match-end-pos)
                                      (- match-end-pos vlfi-start-pos)
                                    (point-min)))
                       (progress-reporter-update search-reporter
                                                 vlfi-end-pos)))))
          (progress-reporter-done search-reporter))
      (if backward
          (vlfi-goto-match match-end-pos match-start-pos
                           count to-find)
        (vlfi-goto-match match-start-pos match-end-pos
                         count to-find)))))

(defun vlfi-goto-match (match-pos-start match-pos-end count to-find)
  "Move to chunk surrounding MATCH-POS-START and MATCH-POS-END.
According to COUNT and left TO-FIND, show if search has been
successful.  Return nil if nothing found."
  (let ((success (zerop to-find)))
    (or success
        (vlfi-move-to-batch (- match-pos-start
                               (/ vlfi-batch-size 2))))
    (let* ((match-end (- match-pos-end vlfi-start-pos))
           (overlay (make-overlay (- match-pos-start vlfi-start-pos)
                                  match-end)))
      (overlay-put overlay 'face 'region)
      (or success (goto-char match-end))
      (prog1 (cond (success t)
                   ((< to-find count)
                    (message "Moved to the %d match which is last"
                             (- count to-find))
                    t)
                   (t (message "Not found")
                      nil))
        (sit-for 0.1)
        (delete-overlay overlay)))))

(defun vlfi-re-search-forward (regexp count)
  "Search forward for REGEXP prefix COUNT number of times.
Search is performed chunk by chunk in `vlfi-batch-size' memory."
  (interactive (list (read-regexp "Search whole file"
                                  (if regexp-history
                                      (car regexp-history))
                                  'regexp-history)
                     (or current-prefix-arg 1)))
  (vlfi-re-search regexp count nil))

(defun vlfi-re-search-backward (regexp count)
  "Search backward for REGEXP prefix COUNT number of times.
Search is performed chunk by chunk in `vlfi-batch-size' memory."
  (interactive (list (read-regexp "Search whole file backward"
                                  (if regexp-history
                                      (car regexp-history))
                                  'regexp-history)
                     (or current-prefix-arg 1)))
  (vlfi-re-search regexp count t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; editing

(defvar vlfi-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'vlfi-write)
    (define-key map "\C-c\C-q" 'vlfi-discard-edit)
    map)
  "Keymap for command `vlfi-edit-mode'.")

(define-derived-mode vlfi-edit-mode vlfi-mode "VLFI[edit]"
  "Major mode for editing large file chunks."
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  (message (substitute-command-keys
            "Editing: Type \\[vlfi-write] to write chunk \
or \\[vlfi-discard-edit] to discard changes.")))

(defun vlfi-discard-edit ()
  "Discard edit and refresh chunk from file."
  (interactive)
  (vlfi-move-to-chunk vlfi-start-pos vlfi-end-pos)
  (vlfi-mode)
  (message "Switched to VLFI mode."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; saving

(defun vlfi-write ()
  "Write current chunk to file.  Always return true to disable save.
If changing size of chunk shift remaining file content."
  (interactive)
  (when (and (derived-mode-p 'vlfi-mode)
             (buffer-modified-p)
             (or (verify-visited-file-modtime)
                 (y-or-n-p "File has changed since visited or saved.  \
Save anyway? ")))
    (let ((size-change (- vlfi-end-pos vlfi-start-pos
                          (length (encode-coding-region
                                   (point-min) (point-max)
                                   buffer-file-coding-system t))))
          (pos (point)))
      (cond ((zerop size-change)
             (write-region nil nil buffer-file-name vlfi-start-pos t))
            ((< 0 size-change)
             (vlfi-file-shift-back size-change))
            (t (vlfi-file-shift-forward (- size-change))))
      (goto-char pos))
    (vlfi-move-to-chunk vlfi-start-pos vlfi-end-pos)
    (vlfi-mode)
    t))

(defun vlfi-file-shift-back (size-change)
  "Shift file contents SIZE-CHANGE bytes back."
  (let ((coding-system buffer-file-coding-system))
    (set-buffer-file-coding-system 'raw-text t)
    (write-region nil nil buffer-file-name vlfi-start-pos t)
    (setq buffer-file-coding-system nil)
    (let ((read-start-pos vlfi-end-pos)
          (reporter (make-progress-reporter "Adjusting file content"
                                            vlfi-end-pos
                                            vlfi-file-size)))
      (while (vlfi-shift-batch read-start-pos (- read-start-pos
                                                 size-change))
        (setq read-start-pos (+ read-start-pos vlfi-batch-size))
        (progress-reporter-update reporter read-start-pos))
      ;; pad end with space
      (erase-buffer)
      (insert-char 32 size-change)
      (write-region nil nil buffer-file-name (- vlfi-file-size
                                                size-change))
      (progress-reporter-done reporter))
    (setq buffer-file-coding-system coding-system)))

(defun vlfi-shift-batch (read-pos write-pos)
  "Read `vlfi-batch-size' bytes from READ-POS and write them \
back at WRITE-POS.  Return nil if EOF is reached, t otherwise."
  (erase-buffer)
  (setq vlfi-file-size (nth 7 (file-attributes buffer-file-name)))
  (let ((read-end (+ read-pos vlfi-batch-size)))
    (insert-file-contents-literally buffer-file-name nil
                                    read-pos
                                    (min vlfi-file-size read-end))
    (write-region nil nil buffer-file-name write-pos t)
    (< read-end vlfi-file-size)))

(defun vlfi-file-shift-forward (size-change)
  "Shift file contents SIZE-CHANGE bytes forward.
Done by saving content up front and then writing previous batch."
  (let ((vlfi-buffer (current-buffer))
        (temp-buffer (generate-new-buffer (concat " "
                                                  (buffer-name))))
        (coding-system buffer-file-coding-system))
    (let ((file buffer-file-name))
      (set-buffer temp-buffer)
      (setq buffer-file-name file))
    (set-buffer vlfi-buffer)
    (set-buffer-file-coding-system 'raw-text t)
    (let ((read-buffer temp-buffer)
          (write-buffer vlfi-buffer)
          (size (+ vlfi-batch-size size-change))
          (read-pos vlfi-end-pos)
          (write-pos vlfi-start-pos)
          swap-buffer
          (reporter (make-progress-reporter "Adjusting file content"
                                            vlfi-start-pos
                                            vlfi-file-size)))
      (while (vlfi-shift-batches size read-buffer read-pos
                                 write-buffer write-pos)
        (setq swap-buffer read-buffer
              read-buffer write-buffer
              write-buffer swap-buffer
              write-pos (+ read-pos size-change)
              read-pos (+ read-pos size))
        (progress-reporter-update reporter write-pos))
      (progress-reporter-done reporter))
    (kill-buffer temp-buffer)
    (set-buffer vlfi-buffer)
    (setq buffer-file-coding-system coding-system)))

(defun vlfi-shift-batches (size read-buffer read-pos
                                write-buffer write-pos)
  "Read SIZE bytes in READ-BUFFER starting from READ-POS.
Then write contents of WRITE-BUFFER to buffer file at WRITE-POS.
Return nil if EOF is reached, t otherwise."
  (let* ((file-size (nth 7 (file-attributes buffer-file-name)))
         (read-more (< read-pos file-size)))
    (when read-more
      ;; read
      (set-buffer read-buffer)
      (erase-buffer)
      (setq buffer-file-coding-system nil)
      (insert-file-contents-literally buffer-file-name nil read-pos
                                      (min file-size (+ read-pos
                                                        size))))
    ;; write
    (set-buffer write-buffer)
    (write-region nil nil buffer-file-name write-pos t)
    read-more))

(provide 'vlfi)

;;; vlfi.el ends here
