;; Custom and other important things

(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)
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

;; Package configuration
(require 'package)
 (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(eval-when-compile (require 'use-package))

(use-package cpputils-cmake
  :init
  (add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))
  )

(use-package flycheck
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save new-line)
  	flycheck-idle-change-delay 5.0
  	flycheck-display-errors-delay 0.9
  	flycheck-standard-error-navigation t)
  )

(setq cppcm-get-executable-full-path-callback
      (lambda (path type tgt-name)
        ;; path is the supposed-to-be target's full path
        ;; type is either add_executabe or add_library
        ;; tgt-name is the target to built. The target's file extension is stripped
        (message "cppcm-get-executable-full-path-callback called => %s %s %s" path type tgt-name)
        (let ((dir (file-name-directory path))
              (file (file-name-nondirectory path)))
          (cond
           ((string= type "add_executable")
            (setq path (concat dir "bin/" file)))
           ;; for add_library
           (t (setq path (concat dir "bin/" file)))
           ))
        ;; return the new path
        (message "cppcm-get-executable-full-path-callback called => path=%s" path)
        path))

;; Keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") `repeat)

(global-unset-key (kbd "M-s"))
(global-set-key (kbd "M-s") `save-buffer)

(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "C-u") `undo)

(global-set-key (kbd "M-n")
		(lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p")
		(lambda () (interactive) (previous-line 5)))

(global-set-key (kbd "s-c") `save-buffers-kill-emacs)
(global-set-key (kbd "s-f") `find-file)
(global-set-key (kbd "s-s") `save-some-buffers)
(global-set-key (kbd "s-0") `delete-window)
(global-set-key (kbd "s-1") `delete-other-windows)
(global-set-key (kbd "s-2") `split-window-vertically)
(global-set-key (kbd "s-3") `split-window-horizontally)
(global-set-key (kbd "s-b") `switch-to-buffer)
(global-set-key [s-tab] (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "s-(") `start-kbd-macro)
(global-set-key (kbd "s-)") `end-kbd-macro)
(global-set-key (kbd "s-e") `call-last-kbd-macro)
(global-set-key (kbd "s-k") `kill-buffer)
(global-set-key (kbd "s-h") `mark-whole-buffer)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, COPY a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, KILL a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Killed line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Formatting
(defun reformat-code ()
  (interactive)
  (defvar astyle-x)
  (defvar astyle-y)
  (setq astyle-x (line-number-at-pos)
        astyle-y (line-number-at-pos (point-max)))
  (shell-command-on-region (point-min) (point-max) 
                           "astyle" t t)
  (with-no-warnings (goto-line astyle-x)))
(global-set-key (kbd "s-a") `reformat-code)
(setq-default c-basic-offset 4)

(defun electric-pair ()
      "If at end of line, insert character pair without surrounding spaces.
    Otherwise, just insert the typed character."
      (interactive)
      (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))
      (add-hook 'c++-mode-hook
              (lambda ()
                (define-key c++-mode-map "(" 'electric-pair)
                (define-key c++-mode-map "[" 'electric-pair)
                (define-key c++-mode-map "{" 'electric-pair)))

(setq column-number-mode t)
(use-package column-marker
  :init
  (add-hook 'foo-mode-hook (lambda () (interactive) (column-marker-1 80))))

;; Themes and other aesthetic tweaks
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

(set-face-attribute 'default nil :height 150)

(set-default 'truncate-lines t)
(setq-default cursor-type 'bar)
(delete-selection-mode 1)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(use-package sublimity)
(use-package sublimity-scroll)
;(use-package sublimity-map)
(use-package sublimity-attractive)
(sublimity-mode 1)

(provide '.emacs)
;;; .emacs ends here
