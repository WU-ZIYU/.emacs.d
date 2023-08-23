;;; remove -- functions to remove something
;;; Commentary:
;;; Code:

(defun kill-clipboard-one-line ()
  (interactive)
  (clipboard-kill-region (point-at-bol) (point-at-eol)))

(defun kill-one-line ()
  (interactive)
  (kill-region (point-at-bol) (point-at-eol)))

(provide 'remove)

;;; remove.el ends here
