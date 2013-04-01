;;; vlfi.el --- View Large Files Improved
;;; -*- lexical-bind: t -*-

;; Copyright (C) 2006, 2012, 2013  Free Software Foundation, Inc.

;; Version: 0.4
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
;; The buffer uses VLFI mode, which defines the commands M-<next>
;; (vlfi-next-batch) and M-<prior> (vlfi-prev-batch) to visit other
;; parts of the file.  The option `vlfi-batch-size' specifies the size
;; of each batch, in bytes.

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

;; Keep track of file position.
(defvar vlfi-start-pos)
(defvar vlfi-end-pos)
(defvar vlfi-file-size)

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
    (define-key map ">" (lambda () "Jump to end of file content."
                          (interactive)
                          (vlfi-insert-file buffer-file-name t)))
    (define-key map "<" (lambda () "Jump to beginning of file content."
                          (interactive)
                          (vlfi-insert-file buffer-file-name)))
    map)
  "Keymap for `vlfi-mode'.")

(define-derived-mode vlfi-mode special-mode "VLFI"
  "Mode to browse large files in."
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (make-local-variable 'vlfi-batch-size)
  (make-local-variable 'vlfi-start-pos)
  (make-local-variable 'vlfi-file-size))

(defun vlfi-change-batch-size (decrease)
  "Change the buffer-local value of `vlfi-batch-size'.
Normally, the value is doubled;
with the prefix argument DECREASE it is halved."
  (interactive "P")
  (or (assq 'vlfi-batch-size (buffer-local-variables))
      (error "%s is not local in this buffer" 'vlfi-batch-size))
  (setq vlfi-batch-size
        (if decrease
            (/ vlfi-batch-size 2)
          (* vlfi-batch-size 2)))
  (vlfi-update-buffer-name))

(defun vlfi-format-buffer-name ()
  "Return format for vlfi buffer name."
  (format "%s(%s)[%.2f%%%%](%d)"
          (file-name-nondirectory buffer-file-name)
          (file-size-human-readable vlfi-file-size)
          (/ (* 100 vlfi-end-pos) (float vlfi-file-size))
          vlfi-batch-size))

(defun vlfi-update-buffer-name ()
  "Update the current buffer name."
  (rename-buffer (vlfi-format-buffer-name) t))

(defun vlfi-next-batch (append)
  "Display the next batch of file data.
When prefix argument is supplied and positive
 jump over APPEND number of batches.
When prefix argument is negative
 append next APPEND number of batches to the existing buffer."
  (interactive "p")
  (let ((end (+ vlfi-end-pos (* vlfi-batch-size
                                (abs append)))))
    (when (< vlfi-file-size end)		; re-check file size
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
      (insert-file-contents buffer-file-name nil
                            (if do-append
                                vlfi-end-pos
                              vlfi-start-pos)
                            end)
      (goto-char pos))
    (setq vlfi-end-pos end))
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
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents buffer-file-name nil
                          vlfi-start-pos vlfi-end-pos))
  (set-buffer-modified-p nil)
  (vlfi-update-buffer-name))

(defun vlfi-move-to-chunk (start end)
  "Move to chunk determined by START END."
  (if (< vlfi-file-size end)          ; re-check file size
      (setq vlfi-file-size (nth 7
                                (file-attributes buffer-file-name))))
  (setq vlfi-start-pos (max 0 start)
        vlfi-end-pos (min end vlfi-file-size))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents buffer-file-name nil
                          vlfi-start-pos vlfi-end-pos))
  (set-buffer-modified-p nil)
  (vlfi-update-buffer-name))

(defun vlfi-insert-file (file &optional from-end)
  "Insert first chunk of FILE contents in current buffer.
With FROM-END prefix, start from the back."
  (if from-end
      (setq vlfi-start-pos (max 0 (- vlfi-file-size vlfi-batch-size))
            vlfi-end-pos vlfi-file-size)
    (setq vlfi-start-pos 0
          vlfi-end-pos (min vlfi-batch-size vlfi-file-size)))
  (vlfi-move-to-chunk vlfi-start-pos vlfi-end-pos))

;;;###autoload
(defun vlfi (file &optional from-end)
  "View Large FILE.  With FROM-END prefix, view from the back.
Batches of the file data from FILE will be displayed in a read-only
buffer.  You can customize number of bytes displayed by customizing
`vlfi-batch-size'."
  (interactive "fFile to open: \nP")
  (with-current-buffer (generate-new-buffer "*vlfi*")
    (buffer-disable-undo)
    (setq buffer-file-name file
          vlfi-file-size (nth 7 (file-attributes file)))
    (vlfi-insert-file file from-end)
    (vlfi-mode)
    (switch-to-buffer (current-buffer))))

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
                                   (format "File %s is large (%s): \
%s normally (o), %s with vlfi (v) or abort (a)"
                                           (file-name-nondirectory filename)
                                           (file-size-human-readable size)
                                           op-type op-type)
                                   'face 'minibuffer-prompt)))
                           '(?o ?O ?v ?V ?a ?A))))
         (cond ((memq char '(?o ?O)))
               ((memq char '(?v ?V))
                (vlfi nil filename)
                (error ""))
               ((memq char '(?a ?A))
                (error "Aborted"))))))

;;; hijack `abort-if-file-too-large'
;;;###autoload
(fset 'abort-if-file-too-large 'vlfi-if-file-too-large)

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
         (half-batch (/ vlfi-batch-size 2)))
    (unwind-protect
        (catch 'end-of-file
          (if backward
              (while (not (zerop to-find))
                (cond ((re-search-backward regexp nil t)
                       (setq match-start-pos (+ vlfi-start-pos
                                                (match-beginning 0)))
                       (let ((new-match-end (+ vlfi-start-pos
                                               (match-end 0))))
                         (if (< new-match-end match-end-pos)
                             (setq to-find (1- to-find)
                                   match-end-pos new-match-end))))
                      ((zerop vlfi-start-pos)
                       (throw 'end-of-file nil))
                      (t (vlfi-move-to-batch (- vlfi-start-pos
                                                half-batch))
                         (goto-char (if (< match-start-pos
                                           vlfi-end-pos)
                                        (- match-start-pos
                                           vlfi-start-pos)
                                      (point-max)))
                         (progress-reporter-update search-reporter
                                                   vlfi-start-pos))))
            (while (not (zerop to-find))
              (cond ((re-search-forward regexp nil t)
                     (setq match-end-pos (+ vlfi-start-pos
                                            (match-end 0)))
                     (let ((new-match-start (+ vlfi-start-pos
                                               (match-beginning 0))))
                       (if (< match-start-pos new-match-start)
                           (setq to-find (1- to-find)
                                 match-start-pos new-match-start))))
                    ((= vlfi-end-pos vlfi-file-size)
                     (throw 'end-of-file nil))
                    (t (vlfi-move-to-batch (- vlfi-end-pos half-batch))
                       (goto-char (if (< vlfi-start-pos match-end-pos)
                                      (- match-end-pos vlfi-start-pos)
                                    (point-min)))
                       (progress-reporter-update search-reporter
                                                 vlfi-end-pos)))))
          (progress-reporter-done search-reporter))
      (vlfi-end-search (if backward match-start-pos match-end-pos)
                       count to-find))))

(defun vlfi-end-search (match-pos count to-find)
  "Move to chunk surrounding MATCH-POS.
According to COUNT and left TO-FIND show if search has been
successful.  Return nil if nothing found."
  (vlfi-move-to-batch (- match-pos (/ vlfi-batch-size 2)))
  (goto-char (- match-pos vlfi-start-pos))
  (cond ((zerop to-find) t)
        ((< to-find count)
         (message "Moved to the %d match which is last found"
                  (- count to-find))
         t)
        (t (message "Not found")
           nil)))

(defun vlfi-re-search-forward (regexp count)
  "Search forward for REGEXP prefix COUNT number of times."
  (interactive (list (read-regexp "Search whole file"
                                  (if regexp-history
                                      (car regexp-history))
                                  'regexp-history)
                     (or current-prefix-arg 1)))
  (vlfi-re-search regexp count nil))

(defun vlfi-re-search-backward (regexp count)
  "Search backward for REGEXP prefix COUNT number of times."
  (interactive (list (read-regexp "Search whole file"
                                  (if regexp-history
                                      (car regexp-history))
                                  'regexp-history)
                     (or current-prefix-arg 1)))
  (vlfi-re-search regexp count t))

(provide 'vlfi)

;;; vlfi.el ends here
