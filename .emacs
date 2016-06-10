(setq make-backup-files nil) 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
 (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") `repeat)

(global-unset-key (kbd "M-s"))
(global-set-key (kbd "M-s") `save-buffer)

(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "C-u") `undo)

(set-default 'truncate-lines t)
(setq-default cursor-type 'bar)
(delete-selection-mode 1)

(require 'dash)
(require 's)

(-each
   (-map
      (lambda (item)
      (format "~/.emacs.d/elpa/%s" item))
   (-filter
      (lambda (item) (s-contains? "theme" item))
      (directory-files "~/.emacs.d/elpa/")))
   (lambda (item)
      (add-to-list 'custom-theme-load-path item)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'hc-zenburn t)

(set-face-attribute 'default nil :height 140)
