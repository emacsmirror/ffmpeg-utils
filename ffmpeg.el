;;; ffmpeg.el --- FFmpeg command utilities wrappers -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-10-09 22:19:18 stardiviner>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "25.1") (notifications "1.2"))
;; Package-Version: 0.1
;; Keywords: multimedia
;; homepage: https://github.com/stardiviner/ffmpeg.el

;; ffmpeg.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ffmpeg.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;;; - [M-x ffmpeg-cut-clip]

;;; Code:

(require 'notifications)

(defun ffmpeg--run-command (arglist)
  "Construct ffmpeg command with ARGLIST and BODY."
  (make-process
   :name "ffmpeg"
   :command (append '("ffmpeg") arglist) ; <------------- problem here
   :buffer "*ffmpeg*"
   :sentinel (lambda (_ __)
               (setq mode-line-process nil)
               (notifications-notify :title "ffmpeg.el" :body "ffmpeg cut process finished")
               (message "FFmpeg process finished."))))

(defun ffmpeg-mode-line-running-indicator (msg)
  "Display a running indicator on mode-line."
  (setq mode-line-process
        (concat " "
                (propertize (or msg "ffmpeg running...")
                            'font-lock-face 'mode-line-highlight)))
  (force-mode-line-update t))

;;; NOTE Because ffmpeg command option "-t" accept seconds like 57 as value.
(defun ffmpeg--subtract-timestamps (start-timestamp end-timestamp)
  "Subtract END-TIMESTAMP with START-TIMESTAMP."
  (time-subtract
   (encode-time (parse-time-string
                 (concat "2020-01-01T" end-timestamp)))
   (encode-time (parse-time-string
                 (concat "2020-01-01T" start-timestamp)))))

;; (ffmpeg--subtract-timestamps "00:11:25" "00:12:12")

(defun ffmpeg-cut-clip (input-filename start-timestamp end-timestamp output-filename)
  "Cut clip of media INPUT-FILENAME between START-TIMESTAMP END-TIMESTAMP and output to OUTPUT-FILENAME.

Support read timestamp begin/end range in format like this: 00:17:23 -- 00:21:45."
  (interactive (if (region-active-p)
                   ;; TODO: regexp spec detect
                   (let* ((time-range (split-string
                                       (buffer-substring-no-properties (region-beginning) (region-end))
                                       " -- "))
                          (timestamp-begin (car time-range))
                          (timestamp-end (cadr time-range)))
                     (list
                      (read-file-name "FFmpeg input filename: ")
                      timestamp-begin
                      timestamp-end
                      (read-file-name "FFmpeg output filename: ")))
                 (list
                  (read-file-name "FFmpeg input filename: ")
                  (read-string "FFmpeg start timestamp: ")
                  (read-string "FFmpeg end timestamp: ")
                  (read-file-name "FFmpeg output filename: "))))
  (deactivate-mark)
  (ffmpeg-mode-line-running-indicator "ffmpeg cut video clip")
  (let ((input-type (file-name-extension input-filename))
        (output-type (file-name-extension output-filename)))
    (if output-type
        ;; output filename specified video file extension.
        (if (string-equal output-type input-type) ; if output extension is same as input
            output-filename             ; keep output original extension
          (if (yes-or-no-p "Whether keep output video format extension? ")
              output-filename
            (setq output-filename (format "%s.%s" (file-name-sans-extension output-filename) input-type))))
      ;; output filename has NOT specific video file extension.
      (setq output-filename (format "%s.%s" output-filename input-type)))
    ;; "ffmpeg -i input-filename -ss start-timestamp -t time-timestamp -codec copy output-filename"
    (ffmpeg--run-command
     `("-i" ,input-filename
       "-ss" ,start-timestamp
       "-t" ,(number-to-string (ffmpeg--subtract-timestamps start-timestamp end-timestamp))
       "-codec" "copy"
       ,output-filename))))



(provide 'ffmpeg)

;;; ffmpeg.el ends here
