;;; vlf-occur.el --- Occur-like functionality for VLF  -*- lexical-binding: t -*-

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Keywords: large files, indexing, occur
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
;; This package provides the `vlf-occur' command which builds
;; index of search occurrences in large file just like occur.

;;; Code:

(require 'vlf)

(defvar vlf-occur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'vlf-occur-next-match)
    (define-key map "p" 'vlf-occur-prev-match)
    (define-key map "\C-m" 'vlf-occur-visit)
    (define-key map "\M-\r" 'vlf-occur-visit-new-buffer)
    (define-key map [mouse-1] 'vlf-occur-visit)
    (define-key map "o" 'vlf-occur-show)
    map)
  "Keymap for command `vlf-occur-mode'.")

(define-derived-mode vlf-occur-mode special-mode "VLF[occur]"
  "Major mode for showing occur matches of VLF opened files.")

(defun vlf-occur-next-match ()
  "Move cursor to next match."
  (interactive)
  (if (eq (get-char-property (point) 'face) 'match)
      (goto-char (next-single-property-change (point) 'face)))
  (goto-char (or (text-property-any (point) (point-max) 'face 'match)
                 (text-property-any (point-min) (point)
                                    'face 'match))))

(defun vlf-occur-prev-match ()
  "Move cursor to previous match."
  (interactive)
  (if (eq (get-char-property (point) 'face) 'match)
      (goto-char (previous-single-property-change (point) 'face)))
  (while (not (eq (get-char-property (point) 'face) 'match))
    (goto-char (or (previous-single-property-change (point) 'face)
                   (point-max)))))

(defun vlf-occur-show (&optional event)
  "Visit current `vlf-occur' link in a vlf buffer but stay in the \
occur buffer.  If original VLF buffer has been killed,
open new VLF session each time.
EVENT may hold details of the invocation."
  (interactive (list last-nonmenu-event))
  (let ((occur-buffer (if event
                          (window-buffer (posn-window
                                          (event-end event)))
                        (current-buffer))))
    (vlf-occur-visit event)
    (pop-to-buffer occur-buffer)))

(defun vlf-occur-visit-new-buffer ()
  "Visit `vlf-occur' link in new vlf buffer."
  (interactive)
  (let ((current-prefix-arg t))
    (vlf-occur-visit)))

(defun vlf-occur-visit (&optional event)
  "Visit current `vlf-occur' link in a vlf buffer.
With prefix argument or if original VLF buffer has been killed,
open new VLF session.
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
              (vlf-buffer (get-char-property pos 'buffer))
              (occur-buffer (current-buffer))
              (match-pos (+ (get-char-property pos 'line-pos)
                            pos-relative)))
          (cond (current-prefix-arg
                 (setq vlf-buffer (vlf file))
                 (switch-to-buffer occur-buffer))
                ((not (buffer-live-p vlf-buffer))
                 (or (catch 'found
                       (dolist (buf (buffer-list))
                         (set-buffer buf)
                         (and vlf-mode (equal file buffer-file-name)
                              (setq vlf-buffer buf)
                              (throw 'found t))))
                     (setq vlf-buffer (vlf file)))
                 (switch-to-buffer occur-buffer)))
          (pop-to-buffer vlf-buffer)
          (vlf-move-to-chunk chunk-start chunk-end)
          (goto-char match-pos)))))

(defun vlf-occur (regexp)
  "Make whole file occur style index for REGEXP.
Prematurely ending indexing will still show what's found so far."
  (interactive (list (read-regexp "List lines matching regexp"
                                  (if regexp-history
                                      (car regexp-history)))))
  (if (buffer-modified-p) ;use temporary buffer not to interfere with modifications
      (let ((vlf-buffer (current-buffer))
            (file buffer-file-name)
            (batch-size vlf-batch-size))
        (with-temp-buffer
          (setq buffer-file-name file)
          (set-buffer-modified-p nil)
          (set (make-local-variable 'vlf-batch-size) batch-size)
          (vlf-mode 1)
          (goto-char (point-min))
          (run-hook-with-args 'vlf-before-batch-functions 'occur)
          (vlf-with-undo-disabled
           (vlf-build-occur regexp vlf-buffer))
          (run-hook-with-args 'vlf-after-batch-functions 'occur)))
    (run-hook-with-args 'vlf-before-batch-functions 'occur)
    (let ((start-pos vlf-start-pos)
          (end-pos vlf-end-pos)
          (pos (point)))
      (vlf-with-undo-disabled
       (vlf-beginning-of-file)
       (goto-char (point-min))
       (unwind-protect (vlf-build-occur regexp (current-buffer))
         (vlf-move-to-chunk start-pos end-pos)
         (goto-char pos))))
    (run-hook-with-args 'vlf-after-batch-functions 'occur)))

(defun vlf-build-occur (regexp vlf-buffer)
  "Build occur style index for REGEXP over VLF-BUFFER."
  (let ((tramp-verbose (if (boundp 'tramp-verbose)
                           (min tramp-verbose 2)))
        (case-fold-search t)
        (line 1)
        (last-match-line 0)
        (last-line-pos (point-min))
        (file buffer-file-name)
        (total-matches 0)
        (match-end-pos (+ vlf-start-pos (position-bytes (point))))
        (occur-buffer (generate-new-buffer
                       (concat "*VLF-occur " (file-name-nondirectory
                                              buffer-file-name)
                               "*")))
        (line-regexp (concat "\\(?5:[\n\C-m]\\)\\|\\(?10:"
                             regexp "\\)"))
        (batch-step (/ vlf-batch-size 8))
        (end-of-file nil)
        (reporter (make-progress-reporter
                   (concat "Building index for " regexp "...")
                   vlf-start-pos vlf-file-size)))
    (unwind-protect
        (progn
          (while (not end-of-file)
            (if (re-search-forward line-regexp nil t)
                (progn
                  (setq match-end-pos (+ vlf-start-pos
                                         (position-bytes
                                          (match-end 0))))
                  (if (match-string 5)
                      (setq line (1+ line) ; line detected
                            last-line-pos (point))
                    (let* ((chunk-start vlf-start-pos)
                           (chunk-end vlf-end-pos)
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
                                              'buffer vlf-buffer
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
              (setq end-of-file (= vlf-end-pos vlf-file-size))
              (unless end-of-file
                (let ((batch-move (- vlf-end-pos batch-step)))
                  (vlf-move-to-batch (if (< batch-move match-end-pos)
                                         match-end-pos
                                       batch-move) t))
                (goto-char (if (< vlf-start-pos match-end-pos)
                               (or (byte-to-position (- match-end-pos
                                                        vlf-start-pos))
                                   (point-min))
                             (point-min)))
                (setq last-match-line 0
                      last-line-pos (line-beginning-position))
                (progress-reporter-update reporter vlf-end-pos))))
          (progress-reporter-done reporter))
      (set-buffer-modified-p nil)
      (if (zerop total-matches)
          (progn (with-current-buffer occur-buffer
                   (set-buffer-modified-p nil))
                 (kill-buffer occur-buffer)
                 (message "No matches for \"%s\"" regexp))
        (with-current-buffer occur-buffer
          (goto-char (point-min))
          (insert (propertize
                   (format "%d matches in %d lines for \"%s\" \
in file: %s" total-matches line regexp file)
                   'face 'underline))
          (set-buffer-modified-p nil)
          (forward-char 2)
          (vlf-occur-mode))
        (display-buffer occur-buffer)))))

(provide 'vlf-occur)

;;; vlf-occur.el ends here
