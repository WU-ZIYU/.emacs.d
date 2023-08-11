;;; cat-init -- Echo "Hello, world!"
;;; Commentary:
;;; Code:

(setq init-path "~/.emacs.d/init.el")

(defun cat-init ()
  "Return the content of init.el"
  (interactive)
  (message "%s"
    (with-temp-buffer
      (insert-file-contents init-path)
      (buffer-substring-no-properties
      (point-min)
      (point-max)))))

(defun open-init (x)
  "Open a new window with a new buffer that contains the init.el"
  (interactive "sOpen in read-only mode? y(yes) or n(no)")
  (switch-to-buffer-other-window "*init*")
  (when (string= "y" x) 
    (read-only-mode))
  (switch-to-buffer (find-file-noselect init-path)))
  

(provide 'cat-init)

;;; cat-init.el ends here
