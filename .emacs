                                        ; Custom and other important things

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)
(defalias 'yes-or-no-p 'y-or-n-p)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb"
                              "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf"
                              "8b313e1793da427e90c034dbe74f3ad9092ac291846c0f855908c42a6bda1ff4"
                              default))) 
 '(inhibit-startup-buffer-menu t) 
 '(inhibit-startup-screen t) 
 '(js2-basic-offset 4) 
 '(js2-bounce-indent-p t) 
 '(magit-commit-arguments nil) 
 '(package-selected-packages (quote (iodine-theme irony twilight-bright-theme web-beautify sublimity
                                                  redo+ jekyll-modes jedi hc-zenburn-theme
                                                  flymake-json elisp-format column-marker))) 
 '(tool-bar-mode nil))

;; Package configuration

;;(require 'package)
;;(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;                         ("marmalade" . "https://marmalade-repo.org/packages/")
;;                         ("melpa" . "https://melpa.org/packages/")))
;;(package-initialize)
(setq shell-file-name "/bin/bash")
;;(unless (package-installed-p 'use-package)
;;  (package-refresh-contents)
;;  (package-install 'use-package))

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory)) 
      (bootstrap-version 5)) 
  (unless (file-exists-p bootstrap-file) 
    (with-current-buffer (url-retrieve-synchronously
                          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                          'silent 'inhibit-cookies) 
      (goto-char (point-max)) 
      (eval-print-last-sexp))) 
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq use-package-always-ensure t)
(eval-when-compile 
  (require 'use-package))
(setq straight-use-package-by-default t)

(use-package 
  flycheck 
  :init (add-hook 'prog-mode-hook #'flycheck-mode) 
  :config (setq flycheck-check-syntax-automatically '(save new-line) flycheck-idle-change-delay 5.0
                flycheck-display-errors-delay 0.9 flycheck-standard-error-navigation t) 
  (setq flycheck-disabled-checkers '(rust rust-cargo rust-clippy)))

(global-set-key (kbd "s-x") nil)
(setq mac-command-modifier 'meta)       ; make cmd key do Meta
(setq mac-option-modifier 'super)       ; make opt key do Super
(setq mac-control-modifier 'control)    ; make Control key do Control
(setq ns-function-modifier 'hyper)      ; make Fn key do Hyper

;; Autocomplete
(use-package 
  company 
  :init (add-hook 'after-init-hook 'global-company-mode))

(setq company-idle-delay .3)
(setq company-echo-delay 0)

(use-package 
  company-tabnine 
  :ensure t
  :init
  (add-to-list 'company-backends 'company-tabnine)
)

(use-package 
  go-mode 
  :init (add-hook 'before-save-hook #'gofmt-before-save) 
  (add-hook 'go-mode-hook (lambda () 
                            (set (make-local-variable 'company-backends) 
                                 '(company-go)))))

(use-package 
  company-go 
  :init (setq company-tooltip-limit 20) 
)

(use-package 
  flycheck-gometalinter 
  :ensure t 
  :config (progn (flycheck-gometalinter-setup)))

(use-package 
  cargo 
  :bind (:map cargo-minor-mode-map
              ("s-y" . cargo-process-clippy) 
              ("s-m" . cargo-process-build) 
              ("s-t" . cargo-process-test )))

(use-package 
  flymake-diagnostic-at-point
  :after flymake 
  :config (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode) 
  (setq flymake-diagnostic-at-point-timer-delay 0.9))

(use-package 
  rust-mode 
  :init (add-hook 'rust-mode-hook 'eglot-ensure))

(use-package 
  eglot 
  :bind (:map eglot-mode-map
              ("s-d" . eglot-help-at-point) 
              ("s-e" . xref-find-definitions) 
              ("s-/" . eglot-rename) 
              ("<s-return>" . eglot-code-actions)) 
  :init (add-hook 'c++-mode-hook 'eglot-ensure) 
  :config (setq eglot-put-doc-in-help-buffer t))

;;(use-package
;;  jedi
;;  :init (add-hook 'python-mode-hook 'jedi:setup)
;;  (setq jedi:complete-on-dot t))
(add-hook 'python-mode-hook 'eglot-ensure)
(with-eval-after-load 'python (define-key python-mode-map (kbd "s-r") 'python-shell-send-region) 
                      (define-key python-mode-map (kbd "<C-return>") 'python-shell-send-buffer))

(use-package 
  cython-mode)

(defun recompile-quietly () 
  "Re-compile without changing the window configuration." 
  (interactive) 
  (save-window-excursion (recompile)))

(use-package 
  glsl-mode 
  :init (autoload 'glsl-mode "glsl-mode" nil t) 
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode)) 
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode)) 
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode)) 
  (add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode)) 
  (add-hook 'glsl-mode-hook (lambda () 
                              (irony-mode -1))) 
  (add-hook 'glsl-mode-hook 'auto-complete-mode))

(flycheck-define-checker glsl-lang-validator "A GLSL checker using glslangValidator.
  See URL https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/" 
                         :command ("glslangValidator" source) 
                         :error-patterns ((error 
                                           line-start
                                           "ERROR: "
                                           column
                                           ":"
                                           line
                                           ": "
                                           (message)
                                           line-end)) 
                         :modes glsl-mode)
(add-to-list 'flycheck-checkers 'glsl-lang-validator)

(use-package 
  opencl-mode)

(use-package 
  cuda-mode)

(use-package 
  flymake-json)

(defun my/use-eslint-from-node-modules () 
  (let* ((root (locate-dominating-file (or (buffer-file-name) 
                                           default-directory) "node_modules")) 
         (eslint (and root 
                      (expand-file-name "node_modules/eslint/bin/eslint.js" root)))) 
    (when (and eslint 
               (file-executable-p eslint)) 
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package 
  js2-mode 
  :interpreter ("node" . js-mode) 
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)) 
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint))) 
  (flycheck-add-mode 'javascript-eslint 'js2-mode) 
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(use-package 
  skewer-mode 
  :init (add-hook 'js2-mode-hook 'skewer-mode))

(use-package 
  ac-js2 
  :init (add-hook 'js2-mode-hook 'ac-js2-mode) 
  (setq ac-js2-evaluate-calls t))

(use-package 
  web-beautify 
  :init (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))

(use-package 
  magit 
  :init (add-hook 'after-save-hook 'magit-after-save-refresh-status))

(use-package 
  epg)

;; (load "~/.emacs.d/lisp/PG/generic/proof-site")

(use-package 
  proof-general
  :init (setq proof-splash-seen t)
  (setq proof-three-window-mode-policy 'hybrid)
  (setq proof-script-fly-past-comments t)
  (with-eval-after-load 'coq (define-key coq-mode-map (kbd "s-<return>") #'proof-goto-point) 
                        (define-key coq-mode-map (kbd "M-n") nil) 
                        (define-key coq-mode-map (kbd "M-p") nil) 
                        (define-key coq-mode-map (kbd "s-n")
                          #'proof-assert-next-command-interactive)))

;; Keybindings
(use-package 
  elmacro 
  :init (elmacro-mode))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") `repeat)

(global-unset-key (kbd "M-s"))
(global-set-key (kbd "M-s") `save-buffer)

;;(use-package
;;  redo+)

(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "C-u") `undo)
(global-set-key (kbd "s-u") `undo)
;; (global-set-key (kbd "s-r") `redo)

(defun top-join-line () 
  (interactive) 
  (delete-indentation 1))

(defun next-several-lines () 
  (interactive) 
  (next-line 5))

(defun previous-several-lines () 
  (interactive) 
  (previous-line 5))

(global-unset-key (kbd "M-n"))
(global-unset-key (kbd "M-p"))
(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "C-M-j"))
(global-set-key (kbd "M-j") 'top-join-line)
(global-set-key (kbd "M-n") `next-several-lines)
(global-set-key (kbd "M-p") `previous-several-lines)
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
;(global-set-key [s-tab] 
;                (lambda () 
;                  (interactive) 
;                  (other-window 1)))
(global-set-key (kbd "s-o") 
                (lambda () 
                  (interactive) 
                  (other-window 1)))
(global-set-key (kbd "s-p") 
                (lambda () 
                  (interactive) 
                  (other-window 1)))
(global-set-key (kbd "s-(") `start-kbd-macro)
(global-set-key (kbd "s-)") `end-kbd-macro)
(global-set-key (kbd "s-{") `shrink-window-horizontally)
(global-set-key (kbd "s-}") `enlarge-window-horizontally)
(global-set-key (kbd "s-k") `kill-buffer)
(global-set-key (kbd "s-h") `mark-whole-buffer)
(global-set-key (kbd "s-u") `universal-argument)
(global-set-key (kbd "s-q") `return-to-mark)
(global-set-key (kbd "s-g") `goto-line)
(global-set-key (kbd "s-[") `scroll-down-line)
(global-set-key (kbd "s-]") `scroll-up-line)

(global-set-key (kbd "s-x s-m") `magit-status)

(add-hook 'c-mode-common-hook (lambda () 
                                (define-key c-mode-base-map (kbd "s-m") 'recompile-quietly)))

(defadvice kill-ring-save (before slick-copy activate compile) 
  "When called interactively with no active region, COPY a single line instead." 
  (interactive (if mark-active (list (region-beginning) 
                                     (region-end)) 
                 (message "Copied line") 
                 (list (line-beginning-position) 
                       (line-beginning-position 2)))))
(defadvice kill-region (before slick-cut activate compile) 
  "When called interactively with no active region, KILL a single line instead." 
  (interactive (if mark-active (list (region-beginning) 
                                     (region-end)) 
                 (message "Killed line") 
                 (list (line-beginning-position) 
                       (line-beginning-position 2)))))

;; Formatting
(use-package 
  clang-format)

(use-package 
  elisp-format)

(defun reformat-code () 
  (interactive)
  ;; Add whatever languages you want here
  (when (derived-mode-p 'c++-mode 'c-mode 'cuda-mode 'glsl-mode) 
    (clang-format-buffer)) 
  (when (derived-mode-p 'emacs-lisp-mode) 
    (elisp-format-buffer)) 
  (when (derived-mode-p 'python-mode) 
    (blacken-buffer)) 
  (when (derived-mode-p 'cython-mode) 
    (blacken-buffer)) 
  (when (derived-mode-p 'rust-mode) 
    (rust-format-buffer)) 
  (when (derived-mode-p 'go-mode) 
    (gofmt)))
(global-set-key (kbd "s-a") `reformat-code)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

(use-package blacken
  :after python
  :init (add-hook 'python-mode-hook 'blacken-buffer))

(use-package 
  whitespace-cleanup-mode 
  :init (add-hook 'python-mode-hook 'whitespace-cleanup-mode) 
  (add-hook 'cython-mode-hook 'whitespace-cleanup-mode) 
  (add-hook 'c-mode-hook 'whitespace-cleanup-mode) 
  (add-hook 'c++-mode-hook 'whitespace-cleanup-mode) 
  (add-hook 'rust-mode-hook 'whitespace-cleanup-mode) 
  (add-hook 'java-mode-hook 'whitespace-cleanup-mode))

(use-package 
  smartparens 
  :init (require 'smartparens-config) 
  (show-smartparens-global-mode +1) 
  (smartparens-global-mode 1) 
  (sp-with-modes '(c-mode c++-mode) 
    (sp-local-pair "{" nil 
                   :post-handlers '(("||\n[i]" "RET"))) 
    (sp-local-pair "/*" "*/" 
                   :post-handlers '((" | " "SPC") 
                                    ("* ||\n[i]" "RET")))))
(use-package 
  visual-fill-column)

(setq column-number-mode t)
(use-package 
  column-marker 
  :init (add-hook 'foo-mode-hook (lambda () 
                                   (interactive) 
                                   (column-marker-1 80))))

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
         (line-beginning (progn (beginning-of-line) 
                                (point))) 
         (is-inside-java-doc (progn (goto-char last) 
                                    (if (search-backward "*/" nil t)
                                        ;; there are some comment endings - search forward
                                        (search-forward "/*" last t)
                                      ;; it's the only comment - search backward
                                      (goto-char last) 
                                      (search-backward "/*" nil t)))) 
         (is-inside-oneline-comment (progn (goto-char last) 
                                           (search-backward comment-start line-beginning t))))

    ;; go to last char position
    (goto-char last)

    ;; the point is inside one line comment, insert the comment-start.
    (if is-inside-oneline-comment (progn (newline-and-indent) 
                                         (insert comment-start))
      ;; else we check if it is java-doc style comment.
      (if is-inside-java-doc (progn (newline-and-indent) 
                                    (insert "* "))
        ;; else insert only new-line
        (newline-and-indent)))))
(add-hook 'prog-mode-hook (lambda () 
                            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'glsl-mode) 
                              (local-set-key (kbd "<RET>") 'advanced-return))))


;; Themes and other aesthetic tweaks

(defvar serif-preserve-default-list nil 
  "A list holding the faces that preserve the default family and
  height when TOGGLE-SERIF is used.")
(setq serif-preserve-default-list '(;; LaTeX markup
                                    font-latex-math-face font-latex-sedate-face
                                                         font-latex-warning-face
                                                         ;; org markup
                                                         org-latex-and-related org-meta-line
                                                         org-verbatim org-block-begin-line
                                                         ;; syntax highlighting using font-lock
                                                         font-lock-builtin-face
                                                         font-lock-comment-delimiter-face
                                                         font-lock-comment-face
                                                         font-lock-constant-face font-lock-doc-face
                                                         font-lock-function-name-face
                                                         font-lock-keyword-face
                                                         font-lock-negation-char-face
                                                         font-lock-preprocessor-face
                                                         font-lock-regexp-grouping-backslash
                                                         font-lock-regexp-grouping-construct
                                                         font-lock-string-face font-lock-type-face
                                                         font-lock-variable-name-face
                                                         font-lock-warning-face))

(defun toggle-serif () 
  "Change the default face of the current buffer to use a serif family." 
  (interactive) 
  (when (display-graphic-p) ;; this is only for graphical emacs
    ;; the serif font familiy and height, save the default attributes
    (let ((serif-fam "Crimson") 
          (serif-height 150) 
          (default-fam (face-attribute 'default 
                                       :family)) 
          (default-height (face-attribute 'default 
                                          :height))) 
      (if (not (bound-and-true-p default-cookie)) 
          (progn (make-local-variable 'default-cookie) 
                 (make-local-variable 'preserve-default-cookies-list) 
                 (setq preserve-default-cookies-list nil)
                 ;; remap default face to serif
                 (setq default-cookie (face-remap-add-relative 'default 
                                                               :family serif-fam 
                                                               :height serif-height))
                 ;; keep previously defined monospace fonts the same
                 (dolist (face serif-preserve-default-list) 
                   (add-to-list 'preserve-default-cookies-list (face-remap-add-relative face 
                                                                                        :family
                                                                                        default-fam 
                                                                                        :height
                                                                                        default-height))) 
                 (message "Turned on serif writing font."))
        ;; undo changes
        (progn (face-remap-remove-relative default-cookie) 
               (dolist (cookie preserve-default-cookies-list) 
                 (face-remap-remove-relative cookie)) 
               (setq default-cookie nil) 
               (setq preserve-default-cookies-list nil) 
               (message "Restored default fonts."))))))
(set-frame-parameter (selected-frame) 'alpha '(95 . 90))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))

;; Fix annoying vertical window splitting.
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
(with-eval-after-load "window" 
  (defcustom split-window-below nil 
    "If non-nil, vertical splits produce new windows below." 
    :group 'windows 
    :type 'boolean)
  (defcustom split-window-right nil 
    "If non-nil, horizontal splits produce new windows to the right." 
    :group 'windows 
    :type 'boolean)
  (fmakunbound #'split-window-sensibly)
  (defun split-window-sensibly 
      (&optional 
       window) 
    (setq window (or window 
                     (selected-window))) 
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (split-window window nil (if split-window-right 'left  'right))) 
        (and (window-splittable-p window)
             ;; Split window vertically.
             (split-window window nil (if split-window-below 'above 'below))) 
        (and (eq window (frame-root-window (window-frame window))) 
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding the
             ;; value of `split-width-threshold'.
             (let ((split-width-threshold 0)) 
               (when (window-splittable-p window t) 
                 (split-window window nil (if split-window-right 'left 'right))))))))
(setq-default split-height-threshold  4 split-width-threshold   160) ; the reasonable limit for horizontal splits

(use-package 
  pandoc-mode)

(use-package 
  markdown-mode
  :bind (:map markdown-mode-map
              ("M-n" . next-several-lines) 
              ("M-p" . previous-several-lines) 
              ("s-n" . markdown-next-link) 
              ("s-p" . markdown-previous-link)))
(use-package
  flyspell-popup)

(define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)

(defun wp_mode () 
  (toggle-serif) 
  (visual-line-mode) 
  (pandoc-mode) 
  (flyspell-mode))

(add-hook 'markdown-mode-hook 'wp_mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(require 'dash)
(require 's)

;;(-each (-map (lambda (item)
;;               (format "~/.emacs.d/elpa/%s" item))
;;             (-filter (lambda (item)
;;                        (s-contains? "theme" item))
;;                      (directory-files "~/.emacs.d/elpa/")))
;;  (lambda (item)
;;    (add-to-list 'custom-theme-load-path item)))

(use-package 
  monokai-theme
  :init (load-theme 'monokai t))
(use-package 
  cyberpunk-theme
  :init (load-theme `cyberpunk t))
(use-package 
  hc-zenburn-theme
  :init (load-theme 'hc-zenburn t))
(enable-theme `hc-zenburn)

(set-face-attribute 'default nil 
                    :height 120)

                                        ; font sizes
(global-set-key (kbd "s-=") 
                (lambda () 
                  (interactive) 
                  (let ((old-face-attribute (face-attribute 'default 
                                                            :height))) 
                    (set-face-attribute 'default nil 
                                        :height (+ old-face-attribute 10)))))

(global-set-key (kbd "s--") 
                (lambda () 
                  (interactive) 
                  (let ((old-face-attribute (face-attribute 'default 
                                                            :height))) 
                    (set-face-attribute 'default nil 
                                        :height (- old-face-attribute 10)))))

(set-default 'truncate-lines t)
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)
(delete-selection-mode 1)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(use-package
  sublimity)
(require 'sublimity-attractive)
(sublimity-mode 1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
