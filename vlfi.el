;;; vlfi.el --- View Large Files  -*- lexical-binding: t -*-

;; Copyright (C) 2006, 2012, 2013  Free Software Foundation, Inc.

;; Version: 0.9.1
;; Keywords: large files, utilities
;; Maintainer: Andrey Kotlarski <m00naticus@gmail.com>
;; Authors: 2006 Mathias Dahl <mathias.dahl@gmail.com>
;;          2012 Sam Steingold <sds@gnu.org>
;;          2013 Andrey Kotlarski <m00naticus@gmail.com>
;; URL: https://github.com/m00natic/vlfi

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
;; large file without loading the entire file.
;; The buffer uses VLFI mode, which defines several commands for
;; moving around, searching and editing selected part of file.

;; This package was inspired by a snippet posted by Kevin Rodgers,
;; showing how to use `insert-file-contents' to extract part of a
;; file.

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
(defvar vlfi-end-pos 0 "Absolute position of the visible chunk end.")
(defvar vlfi-file-size 0 "Total size of presented file.")
(defvar vlfi-encode-size 0 "Size in bytes of current batch decoded.")

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
    (define-key map "o" 'vlfi-occur)
    (define-key map "[" 'vlfi-beginning-of-file)
    (define-key map "]" 'vlfi-end-of-file)
    (define-key map "e" 'vlfi-edit-mode)
    (define-key map "j" 'vlfi-jump-to-chunk)
    (define-key map "l" 'vlfi-goto-line)
    map)
  "Keymap for `vlfi-mode'.")

(put 'vlfi-batch-size 'permanent-local t)
(put 'vlfi-start-pos 'permanent-local t)
(put 'vlfi-end-pos 'permanent-local t)
(put 'vlfi-file-size 'permanent-local t)
(put 'vlfi-encode-size 'permanent-local t)

(define-derived-mode vlfi-mode special-mode "VLFI"
  "Mode to browse large files in."
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (buffer-disable-undo)
  (add-hook 'write-file-functions 'vlfi-write nil t)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'vlfi-revert)
  (make-local-variable 'vlfi-batch-size)
  (make-local-variable 'vlfi-start-pos)
  (make-local-variable 'vlfi-end-pos)
  (make-local-variable 'vlfi-file-size)
  (make-local-variable 'vlfi-encode-size))

;;;###autoload
(defun vlfi (file)
  "View Large FILE.
Batches of the file data from FILE will be displayed in a read-only
buffer.  You can customize number of bytes displayed by customizing
`vlfi-batch-size'."
  (interactive "fFile to open: ")
  (with-current-buffer (generate-new-buffer "*vlfi*")
    (set-visited-file-name file)
    (vlfi-mode)
    (setq vlfi-file-size (vlfi-get-file-size buffer-file-name))
    (vlfi-insert-file)
    (switch-to-buffer (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; integration with other packages

;;;###autoload
(defun dired-vlfi ()
  "In Dired, visit the file on this line in VLFI mode."
  (interactive)
  (vlfi (dired-get-file-for-visit)))

;;;###autoload
(eval-after-load "dired"
  '(define-key dired-mode-map "V" 'dired-vlfi))

;;;###autoload
(defadvice abort-if-file-too-large (around vlfi-if-file-too-large
                                           (size op-type
                                                 &optional filename)
                                           compile activate)
  "If file SIZE larger than `large-file-warning-threshold', \
allow user to view file with `vlfi', open it normally, or abort.
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
                (vlfi filename)
                (error ""))
               ((memq char '(?a ?A))
                (error "Aborted"))))))


;; scroll auto batching
(defadvice scroll-up (around vlfi-scroll-up
                             activate compile)
  "Slide to next batch if at end of buffer in `vlfi-mode'."
  (if (and (derived-mode-p 'vlfi-mode)
           (eobp))
      (progn (vlfi-next-batch 1)
             (goto-char (point-min)))
    ad-do-it))

(defadvice scroll-down (around vlfi-scroll-down
                               activate compile)
  "Slide to previous batch if at beginning of buffer  in `vlfi-mode'."
  (if (and (derived-mode-p 'vlfi-mode)
           (bobp))
      (progn (vlfi-prev-batch 1)
             (goto-char (point-max)))
    ad-do-it))

;; non-recent Emacs
(unless (fboundp 'file-size-human-readable)
  (defun file-size-human-readable (file-size)
    "Print FILE-SIZE in MB."
    (format "%.1fMB" (/ file-size 1048576.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities

(defun vlfi-change-batch-size (decrease)
  "Change the buffer-local value of `vlfi-batch-size'.
Normally, the value is doubled;
with the prefix argument DECREASE it is halved."
  (interactive "P")
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

(defun vlfi-get-file-size (file)
  "Get size in bytes of FILE."
  (nth 7 (file-attributes file)))

(defun vlfi-verify-size ()
  "Update file size information if necessary and visited file time."
  (unless (verify-visited-file-modtime (current-buffer))
    (setq vlfi-file-size (vlfi-get-file-size buffer-file-name))
    (set-visited-file-modtime)))

(defun vlfi-insert-file (&optional from-end)
  "Insert first chunk of current file contents in current buffer.
With FROM-END prefix, start from the back."
  (let ((start 0)
        (end vlfi-batch-size))
    (if from-end
        (setq start (- vlfi-file-size vlfi-batch-size)
              end vlfi-file-size)
      (setq end (min vlfi-batch-size vlfi-file-size)))
    (vlfi-move-to-chunk start end)))

(defun vlfi-beginning-of-file ()
  "Jump to beginning of file content."
  (interactive)
  (vlfi-insert-file))

(defun vlfi-end-of-file ()
  "Jump to end of file content."
  (interactive)
  (vlfi-insert-file t))

(defun vlfi-revert (&optional _ignore-auto noconfirm)
  "Revert current chunk.  Ignore _IGNORE-AUTO.
Ask for confirmation if NOCONFIRM is nil."
  (if (or noconfirm
          (yes-or-no-p (format "Revert buffer from file %s? "
                               buffer-file-name)))
      (vlfi-move-to-chunk vlfi-start-pos vlfi-end-pos)))

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
  (vlfi-verify-size)
  (let* ((end (min (+ vlfi-end-pos (* vlfi-batch-size
                                      (abs append)))
                   vlfi-file-size))
         (start (if (< append 0)
                    vlfi-start-pos
                  (- end vlfi-batch-size))))
    (vlfi-move-to-chunk start end)))

(defun vlfi-prev-batch (prepend)
  "Display the previous batch of file data.
When prefix argument is supplied and positive
 jump over PREPEND number of batches.
When prefix argument is negative
 append previous PREPEND number of batches to the existing buffer."
  (interactive "p")
  (if (zerop vlfi-start-pos)
      (error "Already at BOF"))
  (let* ((start (max 0 (- vlfi-start-pos (* vlfi-batch-size
                                            (abs prepend)))))
         (end (if (< prepend 0)
                  vlfi-end-pos
                (+ start vlfi-batch-size))))
    (vlfi-move-to-chunk start end)))

(defun vlfi-move-to-batch (start &optional minimal)
  "Move to batch determined by START.
Adjust according to file start/end and show `vlfi-batch-size' bytes.
When given MINIMAL flag, skip non important operations."
  (vlfi-verify-size)
  (let ((start (max 0 start))
        (end (min (+ vlfi-start-pos vlfi-batch-size)
                  vlfi-file-size)))
    (if (= vlfi-file-size end)          ; re-adjust start
        (setq start (max 0 (- end vlfi-batch-size))))
    (vlfi-move-to-chunk start end minimal)))

(defun vlfi-move-to-chunk (start end &optional minimal)
  "Move to chunk determined by START END.
When given MINIMAL flag, skip non important operations."
  (catch 'abort
    (let ((changed (not (verify-visited-file-modtime
                         (current-buffer))))
          (modified (buffer-modified-p))
          (inhibit-read-only t)
          (start (max 0 start))
          (end (min end vlfi-file-size)))
      (if changed
          (setq vlfi-file-size (vlfi-get-file-size buffer-file-name)))
      (if (or changed
              (<= vlfi-end-pos start)
              (<= end vlfi-start-pos))
          (progn                        ; full chunk renewal
            (if (and modified
                     (not (y-or-n-p
                           "Buffer modified, are you sure? ")))
                (throw 'abort nil))
            (setq vlfi-start-pos start
                  vlfi-end-pos end)
            (let ((pos (position-bytes (point))))
              (erase-buffer)
              (insert-file-contents-literally
               buffer-file-name nil vlfi-start-pos vlfi-end-pos)
              (setq pos (+ pos (vlfi-prepare-chunk)))
              (decode-coding-region (point-min) (point-max)
                                    buffer-file-coding-system)
              (goto-char (or (byte-to-position pos)
                             (point-max))))
            (set-buffer-modified-p nil))
        (if (and modified
                 (or (< end vlfi-end-pos)
                     (< 3 (- start vlfi-start-pos)))
                 (not (y-or-n-p "Buffer modified, are you sure? ")))
            (throw 'abort nil))
        (let* ((pos (+ (position-bytes (point))
                      vlfi-start-pos))
               (adjust-chunk (< start vlfi-start-pos))
               (adjust-encoding (or adjust-chunk
                                    (< vlfi-end-pos end))))
          (if adjust-encoding
              (encode-coding-region (point-min) (point-max)
                                    buffer-file-coding-system))
          (cond ((< end vlfi-end-pos)       ; adjust ends
                 (let ((offset (- end vlfi-start-pos)))
                   (if adjust-encoding
                       (progn (delete-region offset (point-max))
                              (setq vlfi-end-pos end)))
                   (setq offset (byte-to-position offset))
                   (delete-region offset (point-max))
                   (setq vlfi-end-pos (+ vlfi-start-pos
                                         (position-bytes
                                          (1- offset))))))
                ((< vlfi-end-pos end)
                 (goto-char (point-max))
                 (insert-file-contents-literally
                  buffer-file-name nil vlfi-end-pos end)
                 (setq vlfi-end-pos end)))
          (cond ((< start vlfi-start-pos)   ; adjust start
                 (goto-char (point-min))
                 (insert-file-contents-literally
                  buffer-file-name nil start vlfi-start-pos)
                 (setq vlfi-start-pos start))
                ((< 3 (- start vlfi-start-pos))
                 (let ((offset (- start vlfi-start-pos)))
                   (if adjust-encoding
                       (progn (delete-region (point-min) offset)
                              (setq vlfi-start-pos start))
                     (setq offset (byte-to-position offset))
                     (setq vlfi-start-pos (+ vlfi-start-pos
                                             (position-bytes
                                              (1+ offset))))
                     (delete-region (point-min) offset)))
                 (setq vlfi-start-pos start)))
          (when adjust-encoding
            (if adjust-chunk
                (vlfi-prepare-chunk))
            (decode-coding-region (point-min) (point-max)
                                  buffer-file-coding-system))
          (or modified (set-buffer-modified-p nil))
          (goto-char
           (cond ((< pos vlfi-start-pos) (point-min))
                 ((< vlfi-end-pos pos) (point-max))
                 (t (or (byte-to-position (- pos vlfi-start-pos))
                        (point-min)))))))
      (if changed (set-visited-file-modtime)))
    (or minimal (vlfi-update-buffer-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; encoding

(defun vlfi-prepare-chunk ()
  "Apply proper decoding and adjust chunk start if needed.
Return number of bytes moved back for this to happen."
  (let ((status (vlfi-adjust-chunk 'utf-8)))
    (unless (car status)          ; no success with utf-8, auto-detect
      (delete-region (point-min) (+ (point-min) (cdr status)))
      (setq vlfi-start-pos (+ vlfi-start-pos (cdr status))
            status (vlfi-adjust-chunk)))
    (cdr status)))

(defun vlfi-adjust-chunk (&optional encoding)
  "Adjust chunk beginning until content can be properly decoded.
Try with explicit ENCODING if given, otherwise auto-detect.
Return cons \(success-status . number-of-bytes-moved-back\)."
  (setq buffer-file-coding-system
        (or encoding
            (detect-coding-region (point-min) (point-max) t)))
  (let ((shift 0)
        (success nil)
        (chunk-size (- vlfi-end-pos vlfi-start-pos)))
    (while (and (< shift 4)
                (not (setq success (vlfi-decode-status chunk-size)))
                (not (zerop vlfi-start-pos)))
      (goto-char (point-min))
      (insert-file-contents-literally   ; insert 1 byte
       buffer-file-name nil (1- vlfi-start-pos) vlfi-start-pos)
      (setq shift (1+ shift)
            chunk-size (1+ chunk-size)
            vlfi-start-pos (1- vlfi-start-pos)
            buffer-file-coding-system
            (or encoding
                (detect-coding-region (point-min) (point-max) t))))
    (cons success shift)))

(defun vlfi-decode-status (size)
  "Check if decoding followed by encoding results in SIZE bytes."
  (< (abs (- size (setq vlfi-encode-size
                          (length (encode-coding-string
                                   (decode-coding-region
                                    (point-min) (point-max)
                                    buffer-file-coding-system t)
                                   buffer-file-coding-system t)))))
     4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; search

(defun vlfi-re-search (regexp count backward batch-step)
  "Search for REGEXP COUNT number of times forward or BACKWARD.
BATCH-STEP is amount of overlap between successive chunks."
  (assert (< 0 count))
  (let* ((match-chunk-start vlfi-start-pos)
         (match-chunk-end vlfi-end-pos)
         (match-start-pos (+ vlfi-start-pos (position-bytes (point))))
         (match-end-pos match-start-pos)
         (to-find count)
         (reporter (make-progress-reporter
                    (concat "Searching for " regexp "...")
                    (if backward
                        (- vlfi-file-size vlfi-end-pos)
                      vlfi-start-pos)
                    vlfi-file-size)))
    (unwind-protect
        (catch 'end-of-file
          (if backward
              (while (not (zerop to-find))
                (cond ((re-search-backward regexp nil t)
                       (setq to-find (1- to-find)
                             match-chunk-start vlfi-start-pos
                             match-chunk-end vlfi-end-pos
                             match-start-pos (+ vlfi-start-pos
                                                (position-bytes
                                                 (match-beginning 0)))
                             match-end-pos (+ vlfi-start-pos
                                              (position-bytes
                                               (match-end 0)))))
                      ((zerop vlfi-start-pos)
                       (throw 'end-of-file nil))
                      (t (let ((batch-move (- vlfi-start-pos
                                              (- vlfi-batch-size
                                                 batch-step))))
                           (vlfi-move-to-batch
                            (if (< match-start-pos batch-move)
                                (- match-start-pos vlfi-batch-size)
                              batch-move) t))
                         (goto-char (if (< match-start-pos
                                           vlfi-end-pos)
                                        (or (byte-to-position
                                             (- match-start-pos
                                                vlfi-start-pos))
                                            (point-max))
                                      (point-max)))
                         (progress-reporter-update
                          reporter (- vlfi-file-size
                                      vlfi-start-pos)))))
            (while (not (zerop to-find))
              (cond ((re-search-forward regexp nil t)
                     (setq to-find (1- to-find)
                           match-chunk-start vlfi-start-pos
                           match-chunk-end vlfi-end-pos
                           match-start-pos (+ vlfi-start-pos
                                              (position-bytes
                                               (match-beginning 0)))
                           match-end-pos (+ vlfi-start-pos
                                            (position-bytes
                                             (match-end 0)))))
                    ((= vlfi-end-pos vlfi-file-size)
                     (throw 'end-of-file nil))
                    (t (let ((batch-move (- vlfi-end-pos batch-step)))
                         (vlfi-move-to-batch
                          (if (< batch-move match-end-pos)
                              match-end-pos
                            batch-move) t))
                       (goto-char (if (< vlfi-start-pos match-end-pos)
                                      (or (byte-to-position
                                           (- match-end-pos
                                              vlfi-start-pos))
                                          (point-min))
                                    (point-min)))
                       (progress-reporter-update reporter
                                                 vlfi-end-pos)))))
          (progress-reporter-done reporter))
      (if backward
          (vlfi-goto-match match-chunk-start match-chunk-end
                           match-end-pos match-start-pos
                           count to-find)
        (vlfi-goto-match match-chunk-start match-chunk-end
                         match-start-pos match-end-pos
                         count to-find)))))

(defun vlfi-goto-match (match-chunk-start match-chunk-end
                                          match-pos-start
                                          match-pos-end
                                          count to-find)
  "Move to MATCH-CHUNK-START MATCH-CHUNK-END surrounding \
MATCH-POS-START and MATCH-POS-END.
According to COUNT and left TO-FIND, show if search has been
successful.  Return nil if nothing found."
  (if (= count to-find)
      (progn (vlfi-move-to-chunk match-chunk-start match-chunk-end)
             (goto-char (or (byte-to-position (- match-pos-start
                                                 vlfi-start-pos))
                            (point-max)))
             (message "Not found")
             nil)
    (let ((success (zerop to-find)))
      (if success
          (vlfi-update-buffer-name)
        (vlfi-move-to-chunk match-chunk-start match-chunk-end))
      (let* ((match-end (or (byte-to-position (- match-pos-end
                                                 vlfi-start-pos))
                            (point-max)))
             (overlay (make-overlay (byte-to-position
                                     (- match-pos-start
                                        vlfi-start-pos))
                                    match-end)))
        (overlay-put overlay 'face 'match)
        (unless success
          (goto-char match-end)
          (message "Moved to the %d match which is last"
                   (- count to-find)))
        (sit-for 0.1)
        (delete-overlay overlay)
        t))))

(defun vlfi-re-search-forward (regexp count)
  "Search forward for REGEXP prefix COUNT number of times.
Search is performed chunk by chunk in `vlfi-batch-size' memory."
  (interactive (list (read-regexp "Search whole file"
                                  (if regexp-history
                                      (car regexp-history)))
                     (or current-prefix-arg 1)))
  (vlfi-re-search regexp count nil (/ vlfi-batch-size 8)))

(defun vlfi-re-search-backward (regexp count)
  "Search backward for REGEXP prefix COUNT number of times.
Search is performed chunk by chunk in `vlfi-batch-size' memory."
  (interactive (list (read-regexp "Search whole file backward"
                                  (if regexp-history
                                      (car regexp-history)))
                     (or current-prefix-arg 1)))
  (vlfi-re-search regexp count t (/ vlfi-batch-size 8)))

(defun vlfi-goto-line (n)
  "Go to line N.  If N is negative, count from the end of file."
  (interactive "nGo to line: ")
  (let ((start-pos vlfi-start-pos)
        (end-pos vlfi-end-pos)
        (pos (point))
        (success nil))
    (unwind-protect
        (if (< 0 n)
            (progn (vlfi-beginning-of-file)
                   (goto-char (point-min))
                   (setq success (vlfi-re-search "[\n\C-m]" (1- n)
                                                 nil 0)))
          (vlfi-end-of-file)
          (goto-char (point-max))
          (setq success (vlfi-re-search "[\n\C-m]" (- n) t 0)))
      (if success
          (message "Onto line %s" n)
        (vlfi-move-to-chunk start-pos end-pos)
        (goto-char pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; occur

(defvar vlfi-occur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'vlfi-occur-next-match)
    (define-key map "p" 'vlfi-occur-prev-match)
    (define-key map "\C-m" 'vlfi-occur-visit)
    (define-key map [mouse-1] 'vlfi-occur-visit)
    (define-key map "o" 'vlfi-occur-show)
    map)
  "Keymap for command `vlfi-occur-mode'.")

(define-derived-mode vlfi-occur-mode special-mode "VLFI[occur]"
  "Major mode for showing occur matches of VLFI opened files.")

(defun vlfi-occur-next-match ()
  "Move cursor to next match."
  (interactive)
  (if (eq (get-char-property (point) 'face) 'match)
      (goto-char (next-single-property-change (point) 'face)))
  (goto-char (or (text-property-any (point) (point-max) 'face 'match)
                 (text-property-any (point-min) (point)
                                    'face 'match))))

(defun vlfi-occur-prev-match ()
  "Move cursor to previous match."
  (interactive)
  (if (eq (get-char-property (point) 'face) 'match)
      (goto-char (previous-single-property-change (point) 'face)))
  (while (not (eq (get-char-property (point) 'face) 'match))
    (goto-char (or (previous-single-property-change (point) 'face)
                   (point-max)))))

(defun vlfi-occur-show (&optional event)
  "Visit current `vlfi-occur' link in a vlfi buffer but stay in the \
occur buffer.  If original VLFI buffer has been killed,
open new VLFI session each time.
EVENT may hold details of the invocation."
  (interactive (list last-nonmenu-event))
  (let ((occur-buffer (if event
                          (window-buffer (posn-window
                                          (event-end event)))
                        (current-buffer))))
    (vlfi-occur-visit event)
    (pop-to-buffer occur-buffer)))

(defun vlfi-occur-visit (&optional event)
  "Visit current `vlfi-occur' link in a vlfi buffer.
If original VLFI buffer has been killed,
open new VLFI session each time.
EVENT may hold details of the invocation."
  (interactive (list last-nonmenu-event))
  (when event
    (set-buffer (window-buffer (posn-window (event-end event))))
    (goto-char (posn-point (event-end event))))
  (let* ((pos (point))
         (pos-relative (- pos (line-beginning-position) 1))
         (file (get-char-property pos 'file)))
    (if file
        (let ((chunk-start (get-char-property pos 'chunk-start))
              (chunk-end (get-char-property pos 'chunk-end))
              (buffer (get-char-property pos 'buffer))
              (match-pos (+ (get-char-property pos 'line-pos)
                            pos-relative)))
          (or (buffer-live-p buffer)
              (let ((occur-buffer (current-buffer)))
                (setq buffer (vlfi file))
                (switch-to-buffer occur-buffer)))
          (pop-to-buffer buffer)
          (vlfi-move-to-chunk chunk-start chunk-end)
          (goto-char match-pos)))))

(defun vlfi-occur (regexp)
  "Make whole file occur style index for REGEXP.
Prematurely ending indexing will still show what's found so far."
  (interactive (list (read-regexp "List lines matching regexp"
                                  (if regexp-history
                                      (car regexp-history)))))
  (let ((start-pos vlfi-start-pos)
        (end-pos vlfi-end-pos)
        (pos (point)))
    (vlfi-beginning-of-file)
    (goto-char (point-min))
    (unwind-protect (vlfi-build-occur regexp)
      (vlfi-move-to-chunk start-pos end-pos)
      (goto-char pos))))

(defun vlfi-build-occur (regexp)
  "Build occur style index for REGEXP."
  (let ((line 1)
        (last-match-line 0)
        (last-line-pos (point-min))
        (file buffer-file-name)
        (total-matches 0)
        (match-end-pos (+ vlfi-start-pos (position-bytes (point))))
        (occur-buffer (generate-new-buffer
                       (concat "*VLFI-occur " (file-name-nondirectory
                                               buffer-file-name)
                               "*")))
        (line-regexp (concat "\\(?5:[\n\C-m]\\)\\|\\(?10:"
                             regexp "\\)"))
        (batch-step (/ vlfi-batch-size 8))
        (end-of-file nil)
        (reporter (make-progress-reporter
                   (concat "Building index for " regexp "...")
                   vlfi-start-pos vlfi-file-size)))
    (unwind-protect
        (progn
          (while (not end-of-file)
            (if (re-search-forward line-regexp nil t)
                (progn
                  (setq match-end-pos (+ vlfi-start-pos
                                         (position-bytes
                                          (match-end 0))))
                  (if (match-string 5)
                      (setq line (1+ line) ; line detected
                            last-line-pos (point))
                    (let* ((chunk-start vlfi-start-pos)
                           (chunk-end vlfi-end-pos)
                           (vlfi-buffer (current-buffer))
                           (line-pos (line-beginning-position))
                           (line-text (buffer-substring
                                       line-pos (line-end-position))))
                      (with-current-buffer occur-buffer
                        (unless (= line last-match-line) ;new match line
                          (insert "\n:") ; insert line number
                          (let* ((overlay-pos (1- (point)))
                                 (overlay (make-overlay
                                           overlay-pos
                                           (1+ overlay-pos))))
                            (overlay-put overlay 'before-string
                                         (propertize
                                          (number-to-string line)
                                          'face 'shadow)))
                          (insert (propertize line-text ; insert line
                                              'file file
                                              'buffer vlfi-buffer
                                              'chunk-start chunk-start
                                              'chunk-end chunk-end
                                              'mouse-face '(highlight)
                                              'line-pos line-pos
                                              'help-echo
                                              (format "Move to line %d"
                                                      line))))
                        (setq last-match-line line
                              total-matches (1+ total-matches))
                        (let ((line-start (1+
                                           (line-beginning-position)))
                              (match-pos (match-beginning 10)))
                          (add-text-properties ; mark match
                           (+ line-start match-pos (- last-line-pos))
                           (+ line-start (match-end 10)
                              (- last-line-pos))
                           (list 'face 'match
                                 'help-echo
                                 (format "Move to match %d"
                                         total-matches))))))))
              (setq end-of-file (= vlfi-end-pos vlfi-file-size))
              (unless end-of-file
                (let ((batch-move (- vlfi-end-pos batch-step)))
                  (vlfi-move-to-batch (if (< batch-move match-end-pos)
                                          match-end-pos
                                        batch-move) t))
                (goto-char (if (< vlfi-start-pos match-end-pos)
                               (or (byte-to-position (- match-end-pos
                                                        vlfi-start-pos))
                                   (point-min))
                             (point-min)))
                (setq last-match-line 0
                      last-line-pos (line-beginning-position))
                (progress-reporter-update reporter vlfi-end-pos))))
          (progress-reporter-done reporter))
      (if (zerop total-matches)
          (progn (with-current-buffer occur-buffer
                   (set-buffer-modified-p nil))
                 (kill-buffer occur-buffer)
                 (message "No matches for \"%s\"" regexp))
        (with-current-buffer occur-buffer
          (goto-char (point-min))
          (insert (propertize
                   (format "%d matches from %d lines for \"%s\" \
in file: %s" total-matches line regexp file)
                   'face 'underline))
          (set-buffer-modified-p nil)
          (forward-char 2)
          (vlfi-occur-mode))
        (display-buffer occur-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; editing

(defvar vlfi-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'vlfi-write)
    (define-key map "\C-c\C-q" 'vlfi-discard-edit)
    (define-key map "\C-v" vlfi-mode-map)
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
  (set-buffer-modified-p nil)
  (vlfi-move-to-chunk vlfi-start-pos vlfi-end-pos)
  (vlfi-mode)
  (message "Switched to VLFI mode."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; saving

(defun vlfi-write ()
  "Write current chunk to file.  Always return true to disable save.
If changing size of chunk, shift remaining file content."
  (interactive)
  (when (and (buffer-modified-p)
             (or (verify-visited-file-modtime (current-buffer))
                 (y-or-n-p "File has changed since visited or saved.  \
Save anyway? ")))
    (let ((pos (point))
          (size-change (- vlfi-encode-size
                          (setq vlfi-encode-size
                                (length (encode-coding-region
                                         (point-min) (point-max)
                                         buffer-file-coding-system
                                         t))))))
      (cond ((zerop size-change)
             (write-region nil nil buffer-file-name vlfi-start-pos t))
            ((< 0 size-change)
             (vlfi-file-shift-back size-change))
            (t (vlfi-file-shift-forward (- size-change))))
      (vlfi-move-to-chunk vlfi-start-pos vlfi-end-pos)
      (goto-char pos))
    (vlfi-mode))
  t)

(defun vlfi-file-shift-back (size-change)
  "Shift file contents SIZE-CHANGE bytes back."
  (write-region nil nil buffer-file-name vlfi-start-pos t)
  (buffer-disable-undo)
  (let ((read-start-pos vlfi-end-pos)
        (coding-system-for-write 'no-conversion)
        (reporter (make-progress-reporter "Adjusting file content..."
                                          vlfi-end-pos
                                          vlfi-file-size)))
    (while (vlfi-shift-batch read-start-pos (- read-start-pos
                                               size-change))
      (setq read-start-pos (+ read-start-pos vlfi-batch-size))
      (progress-reporter-update reporter read-start-pos))
    ;; pad end with space
    (erase-buffer)
    (vlfi-verify-size)
    (insert-char 32 size-change)
    (write-region nil nil buffer-file-name (- vlfi-file-size
                                              size-change) t)
    (progress-reporter-done reporter)))

(defun vlfi-shift-batch (read-pos write-pos)
  "Read `vlfi-batch-size' bytes from READ-POS and write them \
back at WRITE-POS.  Return nil if EOF is reached, t otherwise."
  (erase-buffer)
  (vlfi-verify-size)
  (let ((read-end (+ read-pos vlfi-batch-size)))
    (insert-file-contents-literally buffer-file-name nil
                                    read-pos
                                    (min vlfi-file-size read-end))
    (write-region nil nil buffer-file-name write-pos 0)
    (< read-end vlfi-file-size)))

(defun vlfi-file-shift-forward (size-change)
  "Shift file contents SIZE-CHANGE bytes forward.
Done by saving content up front and then writing previous batch."
  (buffer-disable-undo)
  (let ((size (+ vlfi-batch-size size-change))
        (read-pos vlfi-end-pos)
        (write-pos vlfi-start-pos)
        (reporter (make-progress-reporter "Adjusting file content..."
                                          vlfi-start-pos
                                          vlfi-file-size)))
    (when (vlfi-shift-batches size read-pos write-pos t)
      (setq write-pos (+ read-pos size-change)
            read-pos (+ read-pos size))
      (progress-reporter-update reporter write-pos)
      (let ((coding-system-for-write 'no-conversion))
        (while (vlfi-shift-batches size read-pos write-pos nil)
          (setq write-pos (+ read-pos size-change)
                read-pos (+ read-pos size))
          (progress-reporter-update reporter write-pos))))
    (progress-reporter-done reporter)))

(defun vlfi-shift-batches (size read-pos write-pos hide-read)
  "Append SIZE bytes of file starting at READ-POS.
Then write initial buffer content to file at WRITE-POS.
If HIDE-READ is non nil, temporarily hide literal read content.
Return nil if EOF is reached, t otherwise."
  (vlfi-verify-size)
  (let ((read-more (< read-pos vlfi-file-size))
        (start-write-pos (point-min))
        (end-write-pos (point-max)))
    (when read-more
      (goto-char end-write-pos)
      (insert-file-contents-literally buffer-file-name nil read-pos
                                      (min vlfi-file-size (+ read-pos
                                                             size))))
    ;; write
    (if hide-read ; hide literal region if user has to choose encoding
        (narrow-to-region start-write-pos end-write-pos))
    (write-region start-write-pos end-write-pos
                  buffer-file-name write-pos 0)
    (delete-region start-write-pos end-write-pos)
    (if hide-read (widen))
    read-more))

(provide 'vlfi)

;;; vlfi.el ends here
