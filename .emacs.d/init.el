(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")

(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(custom-set-variables
 '(req-package-log-level 'debug))

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

(if (eq system-type 'darwin)
    (custom-set-variables

     '(mac-option-key-is-meta nil)
     '(mac-command-key-is-meta t)
     '(mac-command-modifier 'meta)
     '(mac-option-modifier nil)))

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
      (set-frame-font "Monaco 14")
      (set-face-font 'mode-line "Monaco 14")
      (set-face-font 'mode-line-inactive "Monaco 14")
      (set-face-font 'tooltip "Monaco 14"))
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

(if (eq system-type 'darwin)
    (req-package exec-path-from-shell
      :init
      (exec-path-from-shell-initialize)))

(req-package auto-package-update)

(req-package magit
  :bind
  ("C-x g" . magit-status))

(req-package zenburn-theme
  :defer t)

(req-package solarized-theme
  :defer t)

(req-package hl-line
  :config
  (global-hl-line-mode t))

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
  :diminish global-whitespace-mode)

(req-package ws-butler
  :config
  (ws-butler-global-mode)
  (custom-set-variables
   '(ws-butler-keep-whitespace-before-point nil))
  :diminish ws-butler-mode)

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
  :bind (("M-p"      . drag-stuff-up)
         ("M-<up>"   . drag-stuff-up)
         ("M-n"      . drag-stuff-down)
         ("M-<down>" . drag-stuff-down))
  :diminish drag-stuff-mode)

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
  :bind ("M-x" . smex))

(req-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (custom-set-variables
   '(flycheck-idle-change-delay 1.5)
   '(flycheck-check-syntax-automatically '(idle-change mode-enabled)))
  :diminish flycheck-mode)

(req-package flycheck-clojure
  :require flycheck clojure-mode
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-clojure-setup))

(req-package flycheck-gometalinter
  :require flycheck go-mode go-projectile
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup)
  :config
  (custom-set-variables
   '(flycheck-gometalinter-concurrency 4)
   `(flycheck-gometalinter-executable ,(concat go-projectile-tools-path "/bin/gometalinter"))
   ;; Disable flycheck built-in linters
   '(flycheck-gometalinter-disable-linters '("gocyclo" "vet" "vetshadow" "gofmt" "golint" "test" "errcheck" "unconvert"))))

(req-package flycheck-rust
  :require flycheck rust-mode
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
  :require company
  :config
  (company-flx-mode +1)
  (custom-set-variables '(company-flx-limit 15)))

(req-package company-jedi
  :require company python
  :config
  (add-to-list 'company-backends 'company-jedi))

(req-package company-go
  :require company go-mode go-projectile
  :config
  (custom-set-variables
   `(company-go-gocode-command ,(concat go-projectile-tools-path "/bin/gocode"))))

(req-package company-quickhelp
  :require company
  :config
  (custom-set-variables '(company-quickhelp-delay 0.05)))

(req-package company
  :init
  (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (custom-set-variables '(company-idle-delay 0.3))
  :bind (:map company-active-map
              ("<tab>" . company-complete-common-or-cycle))
  :diminish company-mode)

(req-package smart-tabs-mode
  :config
  (smart-tabs-insinuate 'c 'c++))

(req-package smartparens
  :config
  (sp-use-paredit-bindings)
  :diminish smartparens-mode)

(req-package elpy
  :require python
  :init
  (add-hook 'python-mode-hook 'elpy-mode)
  :config
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation))

(req-package python
  :mode (("\\.py$"      . python-mode)
         ("wscript$"    . python-mode)
         ("SConstruct$" . python-mode)
         ("SConscript$" . python-mode)))

(req-package eldoc
  :diminish eldoc-mode)

(req-package lisp-mode
  :mode ("\\.el$" . emacs-lisp-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)))
  :bind ("M-." . find-function-at-point)
  :interpreter ("emacs" . emacs-lisp-mode))

(req-package markdown-mode
  :mode ("\\.md$" "\\.mdown$" "\\.markdown$"))

(req-package cider
  :require clojure-mode)

(req-package clj-refactor
  :require cider)

(req-package clojure-mode
  :mode (("\\.clj$"  . clojure-mode)
         ("\\.cljc$" . clojurec-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljx$" . clojurex-mode)
         ("\\.edn$"  . clojure-mode))
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
  :mode (("\\.c$"   . c++-mode)
         ("\\.cc$"  . c++-mode)
         ("\\.cpp$" . c++-mode)
         ("\\.cxx$" . c++-mode)
         ("\\.h$"   . c++-mode)
         ("\\.hh$"  . c++-mode)
         ("\\.hpp$" . c++-mode)
         ("\\.hxx$" . c++-mode))
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

(req-package go-snippets
  :require go-mode)

(req-package go-projectile
  :require go-mode
  :config
  (mapc (lambda (tool)
          (add-to-list 'go-projectile-tools tool))
        '((aligncheck   . "github.com/opennota/check/cmd/aligncheck")
          (deadcode     . "github.com/tsenart/deadcode")
          (dupl         . "github.com/mibk/dupl")
          (gas          . "github.com/HewlettPackard/gas")
          (goconst      . "github.com/jgautheron/goconst/cmd/goconst")
          (gocyclo      . "github.com/alecthomas/gocyclo")
          (godoc        . "golang.org/x/tools/cmd/godoc")
          (godoctor     . "github.com/godoctor/godoctor")
          (goimpl       . "github.com/sasha-s/goimpl/cmd/goimpl")
          (goimports    . "golang.org/x/tools/cmd/goimports")
          (gometalinter . "github.com/alecthomas/gometalinter")
          (gosimple     . "honnef.co/go/simple/cmd/gosimple")
          (gotype       . "golang.org/x/tools/cmd/gotype")
          (ineffassign  . "github.com/gordonklaus/ineffassign")
          (interfacer   . "github.com/mvdan/interfacer/cmd/interfacer")
          (lll          . "github.com/walle/lll/cmd/lll")
          (misspell     . "github.com/client9/misspell/cmd/misspell")
          (staticcheck  . "honnef.co/go/staticcheck/cmd/staticcheck")
          (structcheck  . "github.com/opennota/check/cmd/structcheck")
          (unconvert    . "github.com/mdempsky/unconvert")
          (unused       . "honnef.co/go/unused/cmd/unused")
          (varcheck     . "github.com/opennota/check/cmd/varcheck")))
  (let* ((system-path   (getenv "PATH"))
         (go-tools-path (concat go-projectile-tools-path "/bin")))
    (setenv "PATH" (concat system-path ":" go-tools-path)))
  (custom-set-variables
   `(flycheck-go-errcheck-executable ,(concat go-projectile-tools-path "/bin/errcheck"))
   `(flycheck-go-golint-executable ,(concat go-projectile-tools-path "/bin/golint"))
   `(flycheck-go-unconvert-executable ,(concat go-projectile-tools-path "/bin/unconvert"))
   `(godef-command ,(concat go-projectile-tools-path "/bin/godef"))
   `(godoc-command ,(concat go-projectile-tools-path "/bin/godoc"))
   `(gofmt-command ,(concat go-projectile-tools-path "/bin/goimports"))))

(req-package godoctor
  :require go-mode go-projectile
  :config
  (custom-set-variables
   `(godoctor-executable ,(concat go-projectile-tools-path "/bin/godoctor"))))

(req-package go-impl
  :require go-mode)

(req-package go-rename
  :require go-mode go-projectile
  :config
  (custom-set-variables
   `(go-rename-command ,(concat go-projectile-tools-path "/bin/gorename"))))

(req-package go-guru
  :require go-mode go-projectile
  :config
  (custom-set-variables
   `(go-guru-command ,(concat go-projectile-tools-path "/bin/guru"))))

(defconst custom-go-style
  '((tab-width . 2)))

(req-package go-mode
  :mode "\\.go$"
  :config
  (c-add-style "custom-go-style" custom-go-style)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-to-list 'company-backends 'company-go)
  :bind
  (("C-c C-r" . go-remove-unused-imports)
   ("C-c i"   . go-goto-imports)
   ("M-."     . godef-jump)))

(req-package racer
  :require rust-mode
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  :config
  ;; source installed via: rustup component add rust-src
  (let* ((sysroot (string-trim (shell-command-to-string "rustc --print sysroot")))
         (srcpath (concat sysroot "/lib/rustlib/src/rust/src")))
    (custom-set-variables
     `(racer-rust-src-path ,srcpath)))
  :diminish racer-mode)

(req-package cargo
  :require rust-mode
  :diminish cargo-minor-mode
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(req-package rust-mode
  :mode "\\.rs$"
  :config
  (custom-set-variables
   '(rust-format-on-save t)))

(req-package groovy-mode
  :mode ("\\.groovy$" "\\.gradle$"))

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

(load-theme 'zenburn t)
