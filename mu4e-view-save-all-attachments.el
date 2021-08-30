;;; mu4e-view-save-all-attachments.el -- Save all attachments from view mode.
;;; Stephen J Eglen 2021


;; I've created this based on the work of Phil Jackson that required
;; an older version of mu4e.  This version requires the GNUS article
;; code for reading mu4e messages.
;; https://gist.github.com/philjackson/aecfab1706f05079aec7000e328fd183

(defvar bulk-saved-attachments-dir (expand-file-name "~/txt/t/mu4e"))

(defun cleanse-subject (sub)
  (replace-regexp-in-string
   "[^A-Z0-9]+"
   "-"
   (downcase sub)))

(defun mu4e-view-save-all-attachments (&optional arg)
  "Save all MIME parts from current mu4e gnus view buffer."
  ;; Copied from mu4e-view-save-attachments
  (interactive "P")
  (cl-assert (and (eq major-mode 'mu4e-view-mode)
                  (derived-mode-p 'gnus-article-mode)))
  (let* ((msg (mu4e-message-at-point))
         (id (cleanse-subject (mu4e-message-field msg :subject)))
         (attachdir (concat bulk-saved-attachments-dir "/" id))
	 (parts (mu4e~view-gather-mime-parts))
         (handles '())
         (files '())
         dir)
    (mkdir attachdir t)
    (dolist (part parts)
      (let ((fname (or 
		    (cdr (assoc 'filename (assoc "attachment" (cdr part))))
                    (seq-find #'stringp
                              (mapcar (lambda (item) (cdr (assoc 'name item)))
                                      (seq-filter 'listp (cdr part)))))))
        (when fname
          (push `(,fname . ,(cdr part)) handles)
          (push fname files))))
    (if files
        (progn
          (setq dir
		(if arg (read-directory-name "Save to directory: ")
		  attachdir))
          (cl-loop for (f . h) in handles
                   when (member f files)
                   do (mm-save-part-to-file h (expand-file-name f dir))))
      (mu4e-message "No attached files found"))))
