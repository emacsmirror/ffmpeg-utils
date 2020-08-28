;;; ffmpeg.el --- FFmpeg command utilities wrappers -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-08-28 18:07:26 stardiviner>

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
               (notifications-notify :title "ffmpeg.el" :body "ffmpeg cut process finished")
               (message "FFmpeg process finished."))))

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
  "Cut clip of media INPUT-FILENAME between START-TIMESTAMP END-TIMESTAMP and output to OUTPUT-FILENAME."
  (interactive (list
                (read-file-name "FFmpeg input filename: ")
                (read-string "FFmpeg start timestamp: ")
                (read-string "FFmpeg end timestamp: ")
                (read-file-name "FFmpeg output filename: ")))
  ;; "ffmpeg -i input-filename -ss start-timestamp -t time-timestamp -codec copy output-filename"
  (ffmpeg--run-command
   `("-i" ,input-filename
     "-ss" ,start-timestamp
     "-t" ,(number-to-string (ffmpeg--subtract-timestamps start-timestamp end-timestamp))
     "-codec" "copy"
     ,output-filename)))



(provide 'ffmpeg)

;;; ffmpeg.el ends here
