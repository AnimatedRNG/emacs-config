(setq package-enable-at-startup nil)

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
:ref nil
:depth 1
:inherit ignore
:files (:defaults "elpaca-test.el" (:exclude "extensions"))
:build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
(build (expand-file-name "elpaca/" elpaca-builds-directory))
(order (cdr elpaca-order))
(default-directory repo))
(add-to-list 'load-path (if (file-exists-p build) build repo))
(unless (file-exists-p repo)
(make-directory repo t)
(when (<= emacs-major-version 28)
(require 'subr-x))
(condition-case-unless-debug err (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
((zerop (apply #'call-process `("git" nil ,buffer t "clone" ,@(when-let* ((depth (plist-get order
:depth)))
(list (format "--depth=%d" depth) "--no-single-branch"))
,(plist-get order
:repo) ,repo))))
((zerop (call-process "git" nil buffer t "checkout" (or (plist-get order
:ref) "--"))))
(emacs (concat invocation-directory invocation-name))
((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch" "--eval" "(byte-recompile-directory \".\" 0 'force)")))
((require 'elpaca))
((elpaca-generate-autoloads "elpaca" repo)))
(progn (message "%s" (buffer-string))
(kill-buffer buffer))
(error "%s" (with-current-buffer buffer (buffer-string))))
((error)
(warn "%s" err)
(delete-directory repo 'recursive))))
(unless (require 'elpaca-autoloads nil t)
(require 'elpaca)
(elpaca-generate-autoloads "elpaca" repo)
(let ((load-source-file-function nil))
(load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package (elpaca-use-package-mode))
(setq use-package-always-ensure t)

;; Auto-save config
(defconst emacs-tmp-dir (format "%s%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package treesit-auto
:config (setq treesit-auto-install 'prompt) ; Ask before installing new grammars
  (global-treesit-auto-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "8b313e1793da427e90c034dbe74f3ad9092ac291846c0f855908c42a6bda1ff4" default)))
'(inhibit-startup-buffer-menu t)
'(inhibit-startup-screen t)
'(magit-commit-arguments nil)
;; '(package-selected-packages (quote (iodine-theme irony twilight-bright-theme web-beautify sublimity redo+ jekyll-modes jedi hc-zenburn-theme flymake-json elisp-format)))
'(tool-bar-mode nil)
'(menu-bar-mode nil)
'(scroll-bar-mode nil))

;; OS-X settings
(setq mac-command-modifier 'meta)       ; make cmd key do Meta
(setq mac-option-modifier 'super)       ; make opt key do Super
(setq mac-control-modifier 'control)    ; make Control key do Control
(setq ns-function-modifier 'hyper)      ; make Fn key do Hyper

;; Autocomplete
(use-package company
:ensure (:wait t)
:init (add-hook 'after-init-hook 'global-company-mode))
(setq company-idle-delay .3)
(setq company-echo-delay 0)

;; ;; Language-specific features

;; Go
(use-package go-mode
:init (add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
(set (make-local-variable 'company-backends)
'(company-go)))))

(use-package company-go
:init (setq company-tooltip-limit 20))

;; Rust
(use-package cargo
:bind (:map cargo-minor-mode-map ("s-y" . cargo-process-clippy)
("s-m" . cargo-process-build)
("s-t" . cargo-process-test )))

(use-package rust-mode)

;; Python
(with-eval-after-load 'python (define-key python-mode-map (kbd "s-r") 'python-shell-send-region)
(define-key python-mode-map (kbd "<C-return>") 'python-shell-send-buffer))

;; CUDA
(use-package cuda-mode)

;; GLSL
(use-package glsl-mode
:init (autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))
(add-hook 'glsl-mode-hook (lambda ()
(irony-mode -1)))
(add-hook 'glsl-mode-hook 'auto-complete-mode))

;; Flymake
(use-package flymake-json)

(use-package eldoc :defer t)
(use-package flymake :defer t)

;; Eglot config
(use-package eglot
:hook (prog-mode . eglot-ensure) ;; This handles all programming modes
  :bind (:map eglot-mode-map ("s-d" . eglot-help-at-point)
("s-e" . xref-find-definitions)
("s-/" . eglot-rename)
("<s-return>" . eglot-code-actions)))

(require 'project)

;; LLM stuff
(use-package minuet
:bind (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
     ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
     ("C-c m" . #'minuet-configure-provider)
:map minuet-active-mode-map
     ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
     ("C-M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
     ("C-M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
     ("TAB" . #'minuet-accept-suggestion) ;; accept whole completion
     ;; Accept the first line of completion, or N lines with a numeric-prefix:
     ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
     ("C-e" . #'minuet-accept-suggestion-line)
("C-g" . #'minuet-dismiss-suggestion))
:init
    ;; if you want to enable auto suggestion.
    ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
    ;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
:config
(setq minuet-provider 'openai-fim-compatible)
(setq minuet-n-completions 1)
(setq minuet-context-window 4096)
(plist-put minuet-openai-fim-compatible-options
:end-point "http://localhost:9090/v1/completions")
(plist-put minuet-openai-fim-compatible-options
:name "Ollama")
(plist-put minuet-openai-fim-compatible-options
:api-key "TERM")
(plist-put minuet-openai-fim-compatible-options
:model "qwen2.5-coder:7b")
(minuet-set-optional-options minuet-openai-fim-compatible-options
:max_tokens 64))

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-show-diff-after-change nil)
  (setq aidermacs-default-chat-mode 'architect))

;; Keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") `repeat)

(global-unset-key (kbd "M-s"))
(global-set-key (kbd "M-s") `save-buffer)


(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "C-u") `undo)
(global-set-key (kbd "s-u") `undo)

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

(defadvice kill-ring-save (before slick-copy activate compile) "When called interactively with no active region, COPY a single line instead." (interactive (if mark-active (list (region-beginning)
(region-end))
(message "Copied line")
(list (line-beginning-position)
(line-beginning-position 2)))))
(defadvice kill-region (before slick-cut activate compile) "When called interactively with no active region, KILL a single line instead." (interactive (if mark-active (list (region-beginning)
(region-end))
(message "Killed line")
(list (line-beginning-position)
(line-beginning-position 2)))))

;; Formatting
(defun reformat-code ()
(interactive)
(when (derived-mode-p 'c++-mode 'c-mode 'cuda-mode 'glsl-mode)
(clang-format-buffer))
(when (derived-mode-p 'emacs-lisp-mode)
(elisp-format-buffer))
(when (derived-mode-p 'python-mode)
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

(use-package whitespace-cleanup-mode
:init (add-hook 'python-mode-hook 'whitespace-cleanup-mode)
(add-hook 'cython-mode-hook 'whitespace-cleanup-mode)
(add-hook 'c-mode-hook 'whitespace-cleanup-mode)
(add-hook 'c++-mode-hook 'whitespace-cleanup-mode)
(add-hook 'java-mode-hook 'whitespace-cleanup-mode))

;; (use-package smartparens
;; :init (require 'smartparens-config)
;; (show-smartparens-global-mode +1)
;; (smartparens-global-mode 1)
;; (sp-with-modes '(c-mode c++-mode)
;; (sp-local-pair "{" nil
;; :post-handlers '(("||\n[i]" "RET")))
;; (sp-local-pair "/*" "*/"
;; :post-handlers '((" | " "SPC")
;; ("* ||\n[i]" "RET")))))
;; (use-package visual-fill-column)
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  ;; Enable show-smartparens-mode to visualize pairs.
  (show-smartparens-global-mode 1)

  ;; Define custom pairs for C-like modes.
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                               ("* ||\n[i]" "RET")))))

(setq column-number-mode t)

;; Ivy/Swiper/Counsel
(use-package counsel
:after ivy
:config (counsel-mode))

(use-package swiper
:after ivy
:bind (("C-s" . swiper-isearch-backward)
("C-r" . swiper-isearch-backward)))

(use-package ivy
:config (setq ivy-use-virtual-buffers t ivy-count-format "%d/%d " enable-recursive-minibuffers t)
(ivy-mode 1)
(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial))

;; Aesthetics

(with-eval-after-load "window" (defcustom split-window-below nil "If non-nil, vertical splits produce new windows below."
:group 'windows
:type 'boolean)
(defcustom split-window-right nil "If non-nil, horizontal splits produce new windows to the right."
:group 'windows
:type 'boolean)
(fmakunbound #'split-window-sensibly)
(defun split-window-sensibly (&optional window)
(setq window (or window (selected-window)))
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

(use-package monokai-theme)
(use-package cyberpunk-theme)
(use-package hc-zenburn-theme
  :config
  (load-theme 'hc-zenburn t))

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

(set-frame-parameter (selected-frame) 'alpha '(95 . 90))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))

(setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

(use-package sublimity
:config (require 'sublimity-attractive)
(sublimity-mode 1))

(provide '.emacs)
