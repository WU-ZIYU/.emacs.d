;;; thrift-exec -- execute thrift command to compile thrift file in current buffer
;;; Commentary:
;;; Code:

(defun run-thrift-command (language path)
  (defvar original-default-directory default-directory)
  (let ((default-directory path)) 
    (shell-command (format "thrift -r -gen %s -out %s %s" language path (buffer-file-name))))
  (setq default-directory original-default-directory))

(defun thrift-exec (language path)
  "Execute thrift command to compile file in current buffer."
  (interactive (let ((language "")
		     (path "")
		     (default-path (file-name-directory (buffer-file-name))))
		 (setq language (read-string "The language target to compile thrift file to: "))
		 (setq path (read-string (format "Output path (default %s): " default-path)))
		 (list language path)))
  (defvar default-path (file-name-directory (buffer-file-name)))
  (if (string= "" language)
      (message "language target must not be empty.")
      (if (string= "" path)
	  (run-thrift-command language default-path)
	  (run-thrift-command language path))))

(provide 'thrift-exec)

;;; thrift-exec.el ends here.
