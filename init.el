;;; init.el --- Where all the magic begins
;; Load up Org Mode and Org Babel for elisp embedded in Org Mode files
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((started-at (current-time))
       (org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))
  ;; load up Org-mode and Org-babel
  (require 'org-install)
  (require 'ob-tangle)
  (message "Loaded Org Mode (%.03fs)" (float-time (time-since started-at))))

;; Load up all literate org-mode files in this directory
(mapc (lambda (filename)
        (let ((started-at (current-time)))
          (org-babel-load-file filename)
          (message "Loaded Org Babel file: %s (%.03fs)"
                   filename (float-time (time-since started-at)))))
      (directory-files dotfiles-dir t "\\.org$"))

;;; init.el ends here
