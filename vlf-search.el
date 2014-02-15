;;; vlf-search.el --- Search functionality for VLF  -*- lexical-binding: t -*-

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Keywords: large files, search
;; Author: Andrey Kotlarski <m00naticus@gmail.com>
;; URL: https://github.com/m00natic/vlfi

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;; This package provides search utilities for dealing with large files
;; in constant memory.

;;; Code:

(require 'vlf)

(defun vlf-re-search (regexp count backward batch-step)
  "Search for REGEXP COUNT number of times forward or BACKWARD.
BATCH-STEP is amount of overlap between successive chunks."
  (if (<= count 0)
      (error "Count must be positive"))
  (run-hook-with-args 'vlf-before-batch-functions 'search)
  (let* ((tramp-verbose (if (boundp 'tramp-verbose)
                            (min tramp-verbose 2)))
         (case-fold-search t)
         (match-chunk-start vlf-start-pos)
         (match-chunk-end vlf-end-pos)
         (match-start-pos (+ vlf-start-pos (position-bytes (point))))
         (match-end-pos match-start-pos)
         (to-find count)
         (font-lock font-lock-mode)
         (reporter (make-progress-reporter
                    (concat "Searching for " regexp "...")
                    (if backward
                        (- vlf-file-size vlf-end-pos)
                      vlf-start-pos)
                    vlf-file-size)))
    (font-lock-mode 0)
    (vlf-with-undo-disabled
     (unwind-protect
         (catch 'end-of-file
           (if backward
               (while (not (zerop to-find))
                 (cond ((re-search-backward regexp nil t)
                        (setq to-find (1- to-find)
                              match-chunk-start vlf-start-pos
                              match-chunk-end vlf-end-pos
                              match-start-pos (+ vlf-start-pos
                                                 (position-bytes
                                                  (match-beginning 0)))
                              match-end-pos (+ vlf-start-pos
                                               (position-bytes
                                                (match-end 0)))))
                       ((zerop vlf-start-pos)
                        (throw 'end-of-file nil))
                       (t (let ((batch-move (- vlf-start-pos
                                               (- vlf-batch-size
                                                  batch-step))))
                            (vlf-move-to-batch
                             (if (< match-start-pos batch-move)
                                 (- match-start-pos vlf-batch-size)
                               batch-move) t))
                          (goto-char (if (< match-start-pos
                                            vlf-end-pos)
                                         (or (byte-to-position
                                              (- match-start-pos
                                                 vlf-start-pos))
                                             (point-max))
                                       (point-max)))
                          (progress-reporter-update
                           reporter (- vlf-file-size
                                       vlf-start-pos)))))
             (while (not (zerop to-find))
               (cond ((re-search-forward regexp nil t)
                      (setq to-find (1- to-find)
                            match-chunk-start vlf-start-pos
                            match-chunk-end vlf-end-pos
                            match-start-pos (+ vlf-start-pos
                                               (position-bytes
                                                (match-beginning 0)))
                            match-end-pos (+ vlf-start-pos
                                             (position-bytes
                                              (match-end 0)))))
                     ((= vlf-end-pos vlf-file-size)
                      (throw 'end-of-file nil))
                     (t (let ((batch-move (- vlf-end-pos batch-step)))
                          (vlf-move-to-batch
                           (if (< batch-move match-end-pos)
                               match-end-pos
                             batch-move) t))
                        (goto-char (if (< vlf-start-pos match-end-pos)
                                       (or (byte-to-position
                                            (- match-end-pos
                                               vlf-start-pos))
                                           (point-min))
                                     (point-min)))
                        (progress-reporter-update reporter
                                                  vlf-end-pos)))))
           (progress-reporter-done reporter))
       (set-buffer-modified-p nil)
       (if font-lock (font-lock-mode 1))
       (if backward
           (vlf-goto-match match-chunk-start match-chunk-end
                           match-end-pos match-start-pos
                           count to-find)
         (vlf-goto-match match-chunk-start match-chunk-end
                         match-start-pos match-end-pos
                         count to-find))
       (run-hook-with-args 'vlf-after-batch-functions 'search)))))

(defun vlf-goto-match (match-chunk-start match-chunk-end
                                         match-pos-start
                                         match-pos-end
                                         count to-find)
  "Move to MATCH-CHUNK-START MATCH-CHUNK-END surrounding\
MATCH-POS-START and MATCH-POS-END.
According to COUNT and left TO-FIND, show if search has been
successful.  Return nil if nothing found."
  (if (= count to-find)
      (progn (vlf-move-to-chunk match-chunk-start match-chunk-end)
             (goto-char (or (byte-to-position (- match-pos-start
                                                 vlf-start-pos))
                            (point-max)))
             (message "Not found")
             nil)
    (let ((success (zerop to-find)))
      (if success
          (vlf-update-buffer-name)
        (vlf-move-to-chunk match-chunk-start match-chunk-end))
      (let* ((match-end (or (byte-to-position (- match-pos-end
                                                 vlf-start-pos))
                            (point-max)))
             (overlay (make-overlay (byte-to-position
                                     (- match-pos-start
                                        vlf-start-pos))
                                    match-end)))
        (overlay-put overlay 'face 'match)
        (unless success
          (goto-char match-end)
          (message "Moved to the %d match which is last"
                   (- count to-find)))
        (unwind-protect (sit-for 3)
          (delete-overlay overlay))
        t))))

(defun vlf-re-search-forward (regexp count)
  "Search forward for REGEXP prefix COUNT number of times.
Search is performed chunk by chunk in `vlf-batch-size' memory."
  (interactive (if (vlf-no-modifications)
                   (list (read-regexp "Search whole file"
                                      (if regexp-history
                                          (car regexp-history)))
                         (or current-prefix-arg 1))))
  (vlf-re-search regexp count nil (/ vlf-batch-size 8)))

(defun vlf-re-search-backward (regexp count)
  "Search backward for REGEXP prefix COUNT number of times.
Search is performed chunk by chunk in `vlf-batch-size' memory."
  (interactive (if (vlf-no-modifications)
                   (list (read-regexp "Search whole file backward"
                                      (if regexp-history
                                          (car regexp-history)))
                         (or current-prefix-arg 1))))
  (vlf-re-search regexp count t (/ vlf-batch-size 8)))

(defun vlf-goto-line (n)
  "Go to line N.  If N is negative, count from the end of file."
  (interactive (if (vlf-no-modifications)
                   (list (read-number "Go to line: "))))
  (run-hook-with-args 'vlf-before-batch-functions 'goto-line)
  (vlf-verify-size)
  (let ((tramp-verbose (if (boundp 'tramp-verbose)
                           (min tramp-verbose 2)))
        (start-pos vlf-start-pos)
        (end-pos vlf-end-pos)
        (pos (point))
        (font-lock font-lock-mode)
        (success nil))
    (font-lock-mode 0)
    (unwind-protect
        (if (< 0 n)
            (let ((start 0)
                  (end (min vlf-batch-size vlf-file-size))
                  (reporter (make-progress-reporter
                             (concat "Searching for line "
                                     (number-to-string n) "...")
                             0 vlf-file-size))
                  (inhibit-read-only t))
              (setq n (1- n))
              (vlf-with-undo-disabled
               (while (and (< (- end start) n)
                           (< n (- vlf-file-size start)))
                 (erase-buffer)
                 (insert-file-contents-literally buffer-file-name
                                                 nil start end)
                 (goto-char (point-min))
                 (while (re-search-forward "[\n\C-m]" nil t)
                   (setq n (1- n)))
                 (vlf-verify-size)
                 (setq start end
                       end (min vlf-file-size
                                (+ start vlf-batch-size)))
                 (progress-reporter-update reporter start))
               (when (< n (- vlf-file-size end))
                 (vlf-move-to-chunk-2 start end)
                 (goto-char (point-min))
                 (setq success (vlf-re-search "[\n\C-m]" n nil 0)))))
          (let ((start (max 0 (- vlf-file-size vlf-batch-size)))
                (end vlf-file-size)
                (reporter (make-progress-reporter
                           (concat "Searching for line -"
                                   (number-to-string n) "...")
                           0 vlf-file-size))
                (inhibit-read-only t))
            (setq n (- n))
            (vlf-with-undo-disabled
             (while (and (< (- end start) n) (< n end))
               (erase-buffer)
               (insert-file-contents-literally buffer-file-name nil
                                               start end)
               (goto-char (point-max))
               (while (re-search-backward "[\n\C-m]" nil t)
                 (setq n (1- n)))
               (setq end start
                     start (max 0 (- end vlf-batch-size)))
               (progress-reporter-update reporter
                                         (- vlf-file-size end)))
             (when (< n end)
               (vlf-move-to-chunk-2 start end)
               (goto-char (point-max))
               (setq success (vlf-re-search "[\n\C-m]" n t 0))))))
      (if font-lock (font-lock-mode 1))
      (unless success
        (vlf-with-undo-disabled
         (vlf-move-to-chunk-2 start-pos end-pos))
        (vlf-update-buffer-name)
        (goto-char pos)
        (message "Unable to find line"))
      (run-hook-with-args 'vlf-after-batch-functions 'goto-line))))

(provide 'vlf-search)

;;; vlf-search.el ends here
