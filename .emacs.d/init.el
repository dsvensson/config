(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")

(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(custom-set-variables
 '(req-package-log-level 'info))

(condition-case err
    (require 'req-package)
  (error (progn
       (package-refresh-contents)
       (package-install 'req-package)
       (require 'req-package))))

(custom-set-variables
 '(inhibit-startup-message t)
 '(inhibit-startup-echo-area-message t)
 '(initial-scratch-message nil)
 '(tab-width 4))

(if (functionp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (functionp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(server-start)
(global-font-lock-mode t)
(column-number-mode t)
(blink-cursor-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Fonts
(if (eq system-type 'darwin)
    (progn
      (set-frame-font "Monaco 12")
      (set-face-font 'mode-line "Monaco 10")
      (set-face-font 'mode-line-inactive "Monaco 8")
      (set-face-font 'tooltip "Monaco 12")
      (custom-set-variables
       '(ns-antialias-text t)))
  (progn
    ;; Proggy Clean:
    ;; http://www.proggyfonts.com/index.php?menu=download
    ;; (set-default-font "ProggyCleanTTSZ 12")
    ;; (set-face-font 'tooltip "ProggyCleanTTSZ 12")
    (set-frame-font "Inconsolata 16")
    (set-face-font 'tooltip "Inconsolata 16")

    ;; Silkscreen:
    ;; http://kottke.org/plus/type/silkscreen
    ;; (set-face-font 'mode-line "Silkscreen 6")
    ;; (set-face-font 'mode-line-inactive "Silkscreen 6")))
    (set-face-font 'mode-line "Inconsolata 16")
    (set-face-font 'mode-line-inactive "Inconsolata 16")))

;; Patch some colors in themes
(defadvice load-theme (after fixup-face activate)
  (let ((color-orange "#ff8900")
        (color-red "#ff0000")
        (color-linum "#92a0a0")
        (color-background nil)
        (color-background-darker nil)
        (color-background-darkest nil))
    (pcase (ad-get-arg 0)
      (`zenburn (progn
                  (setq color-background (face-background 'default))
                  (setq color-background-darker "#353535")
                  (setq color-background-darkest "#353535")))
      (`solarized-light (progn
                          (setq color-background "#fcf6e4")
                          (setq color-background-darker "#e5dfcf")
                          (setq color-background-darkest "#f2ecdb"))))
    (set-face-foreground 'linum color-linum)
    (set-face-underline 'hl-line  nil)
    (set-face-background 'hl-line color-background-darkest)
    (set-face-background 'whitespace-trailing color-red)
    (set-face-background 'whitespace-tab color-background-darker)
    (set-face-foreground 'idle-highlight color-orange)
    (set-face-background 'idle-highlight color-background)
    (set-face-foreground 'show-paren-match-face color-orange)
    (set-face-background 'show-paren-match-face color-background)))

;; clickable http:// links
(if (functionp 'goto-address-mode)
    (add-hook 'find-file-hooks 'goto-address-mode))

(auto-compression-mode t)
(show-paren-mode t)
(which-function-mode t)
(global-subword-mode t)

(req-package auto-package-update)

(req-package magit)

(req-package zenburn-theme)

(req-package solarized-theme)

(req-package hl-line
  :config
  (global-hl-line-mode t))

(req-package linum
  :config
  (global-linum-mode t)
  (defun linum-format-func (line)
    (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
      (propertize (format (format " %%%dd " w) line) 'face 'linum)))
  (setq linum-format 'linum-format-func))

(req-package idle-highlight-mode
  :init
  (add-hook 'prog-mode-hook 'idle-highlight-mode)
  :config
  (custom-set-variables
   '(idle-highlight-idle-time 0.35)))

(req-package whitespace
  :init
  (autoload 'global-whitespace-mode "whitespace")
  :config
  (global-whitespace-mode t)
  (custom-set-variables
   '(whitespace-style '(face tabs trailing space-before-tab)))
  :diminish
  global-whitespace-mode)

(req-package ws-butler
  :config
  (ws-butler-global-mode)
  (custom-set-variables
   '(ws-butler-keep-whitespace-before-point nil))
  :diminish
  ws-butler-mode)

(req-package pretty-symbols
  :config
  (global-prettify-symbols-mode))

(req-package uniquify
  :config
  (custom-set-variables
   '(uniquify-buffer-name-style 'reverse)
   '(uniquify-separator "/")
   '(uniquify-ignore-buffers-re "^\\*")
   '(uniquify-buffer-name-style 'post-forward-angle-brackets)))

(req-package drag-stuff
  :config
  (drag-stuff-global-mode t)
  :bind
  (("M-p"  . drag-stuff-up)
   ("M-<up>" . drag-stuff-up)
   ("M-n" . drag-stuff-down)
   ("M-<down>" . drag-stuff-down))
  :diminish
  drag-stuff-mode)

(req-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (custom-set-variables
   '(ido-use-filename-at-point 'guess)
   '(ido-create-new-buffer 'always)
   '(ido-enable-flex-matching t)))

(req-package smex
  :config
  (smex-initialize)
  :bind
  ("M-x" . smex))

(req-package flycheck
  :config
  (global-flycheck-mode)
  :diminish
  flycheck-mode)

(req-package flycheck-clojure
  :require
  (flycheck)
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-clojure-setup))

(req-package flycheck-rust
  :defer t
  :init
  (add-hook 'rust-mode-hook
            (lambda ()
              (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

(req-package projectile
  :config
  (when (eq system-type 'darwin)
    (custom-set-variables
     '(projectile-tags-command "/opt/local/bin/ctags -Re %s %s"))))

(req-package company-flx
  :config
  (company-flx-mode +1)
  (custom-set-variables '(company-flx-limit 15)))

(req-package company-quickhelp
  :config
  (custom-set-variables '(company-quickhelp-delay 0.05)))

(req-package company
  :require
  (company-quickhelp company-flx)
  :init
  (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (custom-set-variables '(company-idle-delay 0.3))
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-common-or-cycle))
  :diminish
  company-mode)

(req-package company-jedi
  :config (add-to-list 'company-backends 'company-jedi))

(req-package smart-tabs-mode
  :config
  (smart-tabs-insinuate 'c 'c++))

(req-package smartparens
  :config
  (sp-use-paredit-bindings)
  :diminish
  smartparens-mode)

(req-package elpy
  :defer t
  :init
  (add-hook 'python-mode-hook 'elpy-mode)
  :config
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation))

(req-package python
  :require (elpy)
  :mode
  (("\\.py$" . python-mode)
   ("wscript$" . python-mode)
   ("SConstruct$" . python-mode)
   ("SConscript$" . python-mode)))

(req-package emacs-lisp-mode
  :require
  (eldoc flycheck)
  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)))
  :bind
  ("M-." . find-function-at-point)
  :interpreter
  ("emacs" . emacs-lisp-mode)
  :mode
  ("Cask" . emacs-lisp-mode))

(req-package eldoc
  :diminish
  eldoc-mode)

(req-package markdown-mode
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(req-package clojure-mode
  :require
  (cider clj-refactor flycheck-clojure pretty-symbols smartparens)
  :init
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  :config
  (custom-set-variables
   '(clojure--prettify-symbols-alist
     '(("fn"  . ?λ)
       ("!=" . ?≠)
       ("<=" . ?≤)
       (">=" . ?≥)))))

(defconst custom-c-style
  '((tab-width . 4)
    (indent-tabs-mode . t)
    (c-offsets-alist . ((case-label . +)))
    (c-cleanup-list . (space-before-funcall
                       brace-else-brace
                       brace-elseif-brace
                       comment-close-slash
                       defun-close-semi))))

(req-package cc-mode
  :config
  (c-add-style "custom-c-style" custom-c-style)
  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; (add-to-list 'electric-layout-rules '(?{ . around))
              ;; (add-to-list 'electric-pair-pairs '(?{ . ?}))
              (electric-layout-mode 1)
              (electric-pair-mode 1)
              (c-toggle-auto-hungry-state 1)
              (c-set-style "custom-c-style")))
  :bind (:map c-mode-base-map
              ("<return>" . c-context-line-break)))

(defconst custom-go-style
  '((tab-width . 2)))

(req-package go-mode
  :require
  (flycheck flycheck-gometalinter company-go)
  :config
  (c-add-style "custom-go-style" custom-go-style)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-to-list 'company-backends 'company-go))

(req-package racer
  :defer t
  :diminish racer-mode
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  :config
  ;; source installed via: rustup component add rust-src
  (let* ((sysroot (string-trim (shell-command-to-string "rustc --print sysroot")))
         (srcpath (concat sysroot "/lib/rustlib/src/rust/src")))
    (custom-set-variables
     `(racer-rust-src-path ,srcpath)
     '(rust-format-on-save t))))

(req-package cargo
  :defer t
  :diminish cargo-minor-mode
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(req-package rust-mode
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'rust-format-buffer nil t))))

(req-package groovy-mode
  :mode (("\\.groovy$" . groovy-mode)
         ("\\.gradle$" . groovy-mode)))

(req-package windmove
  :bind (("C-s-<left>"  . windmove-left)
         ("C-s-<right>" . windmove-right)
         ("C-s-<up>"    . windmove-up)
         ("C-s-<down>"  . windmove-down)))

(req-package buffer-move
  :bind (("C-s-<s-left>" . buf-move-left)
         ("C-s-<s-right>" . buf-move-right)
         ("C-s-<s-up>" . buf-move-up)
         ("C-s-<s-down>" . buf-move-down)))

(req-package-finish)

(custom-set-variables
 '(auto-save-default nil)
 '(make-backup-files nil)
 '(kill-whole-line t)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1)))
 '(mouse-wheel-follow-mouse 't)
 '(scroll-step 1)
 '(indicate-empty-lines t)
 '(indent-tabs-mode nil)
 '(visible-bell nil))

(defun yank-and-indent ()
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(global-set-key (kbd "<f1>") #'manual-entry)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-u") 'kill-whole-line)
(global-set-key (kbd "C-y") 'yank-and-indent)
(global-set-key [XF86Back] 'previous-buffer)
(global-set-key [XF86Forward] 'next-buffer)

(if (eq system-type 'darwin)
    (custom-set-variables
     '(mac-option-modifier 'meta)
     '(mac-command-modifier 'hyper)))

(load-theme 'zenburn t)
