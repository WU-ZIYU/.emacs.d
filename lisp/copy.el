;;; copy -- copy a word before the cursor
;;; Commentary:
;;; Code:

(defun backward-copy-word ()
  (interactive)
  (save-excursion
    (copy-region-as-kill (point) (progn (backward-word) (point)))))

(defun copy-cursor-word ()
  (interactive)
  (save-excursion
    (copy-region-as-kill (progn (backward-word) (point)) (progn (forward-word) (point)))))

(defun copy-one-line ()
  (interactive)
  (save-excursion)
  (copy-region-as-kill (point-at-bol) (point-at-eol)))

(provide 'copy)
;;; copy.el ends here
