;; Custom and other important things

;; Save all tempfiles in $TMPDIR/emacs$UID/                                                        
(defconst emacs-tmp-dir (format "%s%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
    `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
    `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
    emacs-tmp-dir)
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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(eval-when-compile (require 'use-package))

(use-package flycheck
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save new-line)
  	flycheck-idle-change-delay 5.0
  	flycheck-display-errors-delay 0.9
  	flycheck-standard-error-navigation t)
  )

;; Autocomplete
;(use-package rtags)
;(use-package cmake-ide
;  :init
;  (setq cmake-ide-flags-c++ '("-I/usr/include/c++/6.1.1"
;			    "-I/usr/include/c++/6.1.1/x86_64-pc-linux-gnu"
;			    "-I/usr/include/c++/6.1.1/backward"
;			    "-I/usr/local/include"
;			    "-I/usr/include"
;			    "-I/usr/share/include")))
;(use-package company-mode
;  :init
;  (add-hook 'after-init-hook 'global-company-mode))
;(eval-after-load 'company
;  '(add-to-list 'company-backends 'company-irony))

;(add-hook 'c++-mode-hook 'irony-mode)
;(add-hook 'c-mode-hook 'irony-mode)
;(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
;(defun my-irony-mode-hook ()
;  (define-key irony-mode-map [remap completion-at-point]
;    'irony-completion-at-point-async)
;  (define-key irony-mode-map [remap complete-symbol]
;    'irony-completion-at-point-async))
;(add-hook 'irony-mode-hook 'my-irony-mode-hook)
;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;(cmake-ide-setup)

;; Keybindings
(use-package elmacro
  :init
  (elmacro-mode))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") `repeat)

(use-package god-mode
  :init
  (global-set-key (kbd "<escape>") 'god-mode-all))

(global-unset-key (kbd "M-s"))
(global-set-key (kbd "M-s") `save-buffer)

(use-package redo+)
(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "C-u") `undo)
(global-set-key (kbd "s-u") `undo)
(global-set-key (kbd "s-r") `redo)

(global-unset-key (kbd "M-n"))
(global-unset-key (kbd "M-p"))
(global-set-key (kbd "M-n")
		(lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p")
		(lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "s-n") `next-error)
(global-set-key (kbd "s-p") `previous-error)

(defun return-to-mark ()
        (interactive)
	(set-mark-command '(4)))

(global-set-key (kbd "s-c") `save-buffers-kill-emacs)
(global-set-key (kbd "s-f") `find-file)
(global-set-key (kbd "s-s") `save-buffer)
(global-set-key (kbd "s-0") `delete-window)
(global-set-key (kbd "s-1") `delete-other-windows)
(global-set-key (kbd "s-2") `split-window-vertically)
(global-set-key (kbd "s-3") `split-window-horizontally)
(global-set-key (kbd "s-b") `switch-to-buffer)
(global-set-key [s-tab] (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "s-(") `start-kbd-macro)
(global-set-key (kbd "s-)") `end-kbd-macro)
(global-set-key (kbd "s-{") `shrink-window-horizontally)
(global-set-key (kbd "s-}") `enlarge-window-horizontally)
(global-set-key (kbd "s-e") `call-last-kbd-macro)
(global-set-key (kbd "s-k") `kill-buffer)
(global-set-key (kbd "s-h") `mark-whole-buffer)
(global-set-key (kbd "s-u") `universal-argument)
(global-set-key (kbd "s-q") `return-to-mark)
(global-set-key (kbd "s-x") `exchange-point-and-mark)
(global-set-key (kbd "s-g") `goto-line)

(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map (kbd "s-m") 'compile)))

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
(setq-default indent-tabs-mode nil)

(use-package smartparens
  :init
  (require 'smartparens-config)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1)
  (sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))
)

(setq column-number-mode t)
(use-package column-marker
  :init
  (add-hook 'foo-mode-hook (lambda () (interactive) (column-marker-1 80))))

;; Advanced return for programming.
;; Shuai Li
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun advanced-return ()
  "Advanced `newline' command for comment.  This function redefine <Enter> to
provide a corrent comment symbol at each newline plus a space when you press
<Enter> in the comment.  It also support JavaDoc style comment -- insert a `*'
at the beggining of the new line if inside of a comment."
  (interactive "*")
  (let* ((last (point))
         (line-beginning (progn (beginning-of-line) (point)))
         (is-inside-java-doc
          (progn
            (goto-char last)
            (if (search-backward "*/" nil t)
                ;; there are some comment endings - search forward
                (search-forward "/*" last t)
              ;; it's the only comment - search backward
              (goto-char last)
              (search-backward "/*" nil t)
              )
            )
          )
         (is-inside-oneline-comment
          (progn
            (goto-char last)
            (search-backward comment-start line-beginning t)
            )
          )
         )

    ;; go to last char position
    (goto-char last)

    ;; the point is inside one line comment, insert the comment-start.
    (if is-inside-oneline-comment
        (progn
          (newline-and-indent)
          (insert comment-start)
          )
      ;; else we check if it is java-doc style comment.
      (if is-inside-java-doc
          (progn
            (newline-and-indent)
            (insert "* ")
            )
        ;; else insert only new-line
        (newline-and-indent)
        )
      )
      )
  )
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "<RET>") 'advanced-return)))


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

(use-package monokai-theme
  :init
  (load-theme 'monokai t))
(use-package hc-zenburn-theme
  :init
  (load-theme 'hc-zenburn t)
  (enable-theme `hc-zenburn))

(set-face-attribute 'default nil :height 150)

(set-default 'truncate-lines t)
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)
(delete-selection-mode 1)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(use-package sublimity)
(require 'sublimity-scroll)
(require 'sublimity-attractive)
(sublimity-mode 1)

(provide '.emacs)
;;; .emacs ends here
