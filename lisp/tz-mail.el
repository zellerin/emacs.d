;;; tz-mail.el --- GNUs customizations               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Tomas Zellerin

;; Author: Tomas Zellerin <tomas@zellerin.cz>
;; Keywords: mail, news

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Version: 0.1
;;; Commentary:

;;


;;; Code:
(setq message-send-mail-function 'smtpmail-send-it)
(load "private" t)
(setq
 smtpmail-smtp-server "smtp.zoho.com"
 smtpmail-smtp-service 587

 gnus-select-method '(nnml "")
 gnus-use-adaptive-scoring '(word line)
 gnus-article-mime-part-function 'tz-mail-handle-attachment)

(eval-after-load "nnmail"
  '(setq nnmail-split-methods
	'(("csob" "^From: .*\\(CSOB Administrator\\|tbs\\.csob\\.cz\\|vypisy@hypotecnibanka.cz\\)")
	  ("mail.misc" ""))))

;;;###autoload
(defun tz-mail-handle-attachment (handle)
  "Treat specifically attachments during mail opening."
  (cond
   ;; Offer saving of the jpegs to temp
   ((member (car (mm-handle-type handle)) '("application/pdf" "image/jasdaspeg"))
    (with-temp-buffer
      (insert (mm-get-part handle))
      (write-region (point-min) (point-max)
		    (read-file-name "Save jpeg to: " "/tmp/"))))

   ;; Octet stream may be XML that may be bank statement.
   ;; It needs to be utfized and saved.
   ((equal (car (mm-handle-type handle)) "application/octet-stream")
    (save-excursion
      (with-temp-buffer
         (insert (mm-get-part handle))
         (goto-char 1)
         (search-forward "windows-1250")
         (recode-region (point-min) (point-max)
			'windows-1250 'utf-8-unix )
         (replace-match "utf-8")
         (write-region (point-min) (point-max)
                       (read-file-name "Save data to: " "~/ucty/"
                                       nil nil
                                       (mm-handle-filename handle))))))))



(provide 'tz-mail)
;;; tz-mail.el ends here
