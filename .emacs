(setq make-backup-files nil) 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/lisp/")
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'wc-mode "wc-mode" "Word count module" t)

;; easy spell check
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

(defvar serif-preserve-default-list nil
    "A list holding the faces that preserve the default family and
    height when TOGGLE-SERIF is used.")
  (setq serif-preserve-default-list
        '(;; LaTeX markup
          font-latex-math-face
          font-latex-sedate-face
          font-latex-warning-face
          ;; org markup
          org-latex-and-related
          org-meta-line
          org-verbatim
          org-block-begin-line
          ;; syntax highlighting using font-lock
          font-lock-builtin-face
          font-lock-comment-delimiter-face
          font-lock-comment-face
          font-lock-constant-face
          font-lock-doc-face
          font-lock-function-name-face
          font-lock-keyword-face
          font-lock-negation-char-face
          font-lock-preprocessor-face
          font-lock-regexp-grouping-backslash
          font-lock-regexp-grouping-construct
          font-lock-string-face
          font-lock-type-face
          font-lock-variable-name-face
          font-lock-warning-face))

(defun toggle-serif ()
    "Change the default face of the current buffer to use a serif family."
    (interactive)
    (when (display-graphic-p)  ;; this is only for graphical emacs
      ;; the serif font familiy and height, save the default attributes
      (let ((serif-fam "Liberation Serif")
            (serif-height 120)
            (default-fam (face-attribute 'default :family))
            (default-height (face-attribute 'default :height)))
        (if (not (bound-and-true-p default-cookie))
            (progn (make-local-variable 'default-cookie)
                   (make-local-variable 'preserve-default-cookies-list)
                   (setq preserve-default-cookies-list nil)
                   ;; remap default face to serif
                   (setq default-cookie
                         (face-remap-add-relative
                          'default :family serif-fam :height serif-height))
                   ;; keep previously defined monospace fonts the same
                   (dolist (face serif-preserve-default-list)
                     (add-to-list 'preserve-default-cookies-list
                                  (face-remap-add-relative
                                   face :family default-fam :height default-height)))
                   (message "Turned on serif writing font."))
          ;; undo changes
          (progn (face-remap-remove-relative default-cookie)
                 (dolist (cookie preserve-default-cookies-list)
                   (face-remap-remove-relative cookie))
                 (setq default-cookie nil)
                 (setq preserve-default-cookies-list nil)
                 (message "Restored default fonts."))))))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") `repeat)

(global-unset-key (kbd "M-s"))
(global-set-key (kbd "M-s") `save-buffer)

(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "C-u") `undo)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

(set-face-attribute 'default nil :height 140)
