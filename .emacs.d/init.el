(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/vendor")

;; start the emacs server
(server-start)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

;; enable syntax highlighting
(global-font-lock-mode t)

;; disable menubar, toolbar and scrollbar
(if (functionp 'menu-bar-mode)
    (menu-bar-mode nil))
(if (functionp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(require 'idle-highlight-mode)
(setq idle-highlight-idle-time 0.25)
(set-face-background 'idle-highlight "#505050")

;; show column in statusbar
(column-number-mode t)

;; don't let the cursor blink
(blink-cursor-mode nil)

;; color theme
(when (>= emacs-major-version 23)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (load-theme 'zenburn t))

;; Fonts
(if (eq system-type 'darwin)
    (progn
      (set-default-font "Monaco 12")
      (set-face-font 'mode-line "Monaco 10")
      (set-face-font 'mode-line-inactive "Monaco 8")
      (setq ns-antialias-text t)
      (when (>= emacs-major-version 23)
        (set-face-font 'tooltip "Monaco 12")))
  (progn
    ;; Proggy Clean:
    ;; http://www.proggyfonts.com/index.php?menu=download
    (set-default-font "ProggyCleanTTSZ 12")
    (when (>= emacs-major-version 23)
      (set-face-font 'tooltip "ProggyCleanTTSZ 12"))

    ;; Silkscreen:
    ;; http://kottke.org/plus/type/silkscreen
    (set-face-font 'modeline "Silkscreen 6")
    (set-face-font 'mode-line-inactive "Silkscreen 6")))

;; clickable http:// links
(if (functionp 'goto-address-mode)
    (add-hook 'find-file-hooks 'goto-address-mode))

;; highlight current line
(require 'hl-line)
(global-hl-line-mode t)

;; make highlight line a bit darker than the background color
(set-face-background 'hl-line "#353535")
(set-face-underline 'hl-line  nil)

;; disable welcome message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; highlight trailing whitespace and mixed leading space/tab.
(require 'whitespace)
(autoload 'global-whitespace-mode "whitespace")
(setq whitespace-style '(face tabs trailing space-before-tab))
(global-whitespace-mode t)

(when (>= emacs-major-version 23)
  (set-face-background 'whitespace-tab "#353535")
  (set-face-background 'whitespace-trailing "#ff0000"))

;; set default tab width
(setq tab-width 4)
(setq-default tab-width 4)

;; edit compressed files
(auto-compression-mode t)

;; disable backup files
(setq make-backup-files nil)

;; match parenthesis
(show-paren-mode t)
(set-face-foreground 'show-paren-match-face "#ff5400")

;; show what function i'm in
(which-func-mode t)

;; kill the newline too
(setq kill-whole-line t)

;; http://stupefydeveloper.blogspot.com/2009/04/emacs-proper-kill-whole-line.html
(defun kill-total-line ()
  (interactive)
  (let ((kill-whole-line t))
    (beginning-of-line)
    (kill-line)
    (setq top (car kill-ring))
    (setq last (substring top -1))
    (if (string-equal last "\n")
        (let ()
          (setq stripped (substring top 0 -1))
          (setq kill-ring (cons (cdr kill-ring)))))))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

(setq auto-save-default nil)

;; equivalence to vim dd
(global-set-key (kbd "C-u") 'kill-total-line)

;; F1 for man-pages
(global-set-key (kbd "<f1>") #'manual-entry)

;; short yes/no answers
(fset 'yes-or-no-p 'y-or-n-p)

;; mark lines not in buffer
(setq default-indicate-empty-lines t)

;; switch buffer back and forth with keys
(global-set-key [XF86Back] 'previous-buffer)
(global-set-key [XF86Forward] 'next-buffer)

;; uniquify buffer names if files are named similarily
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; automatically rescan symbols
(require 'imenu)
(setq imenu-auto-rescan t)

;; speedbar
(require 'sr-speedbar)
(setq speedbar-use-images nil)
(setq speedbar-show-unknown-files t)
(setq speedbar-tag-hierarchy-method nil)
(setq speedbar-hide-button-brackets-flag t)
(setq speedbar-use-imenu-flag t)
(setq sr-speedbar-auto-refresh t)
(speedbar-change-initial-expansion-list "buffers")
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; IDO
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

(add-hook 'python-mode-hook '(lambda ()
                               (setq tab-width 4)
                               (setq python-indent 4)))

;; indent when starting a new line
(add-hook 'c-mode-hook '(lambda ()
                          (setq-default c-basic-offset 4)
                          (c-set-style "k&r")
                          (c-set-offset 'case-label '+)
                          (setq c-cleanup-list '(space-before-funcall
                                                 brace-else-brace
                                                 brace-elseif-brace
                                                 comment-close-slash))
                          (local-set-key (kbd "RET") 'c-context-line-break)))

                                        ; automagicalize
(add-hook 'c-mode-common-hook '(lambda ()
                                 (c-toggle-auto-hungry-state 1)))

;; handle CamelCase properly
(when (>= emacs-major-version 24)
  (global-subword-mode t))

;; yank and then indent the newly formed region according to mode.
(defun yank-and-indent ()
  (interactive)
  (yank)
  (call-interactively 'indent-region))
(global-set-key "\C-y" 'yank-and-indent)

;; default to regexp searches
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)

;; automatically select python for SCons/waf files
(add-to-list 'auto-mode-alist '("wscript" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))

(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))

(autoload 'xs-mode "xs-mode" "Major mode for XS files" t)
(add-to-list 'auto-mode-alist '("\\.xs$" . xs-mode))

;; smart tabs
(require 'smarttabs)

;; automatically nuke all trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; flyspell comments
(add-hook 'c-mode-hook 'flyspell-prog-mode)

;; pyflakes static analysis of python code
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;; disable flymake message box on failure
(setq-default flymake-gui-warnings-enabled nil)

(autoload 'espresso-mode "espresso")

(defun custom-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion
        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))
        (if (looking-at "default:")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun custom-js2-indent ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun customize-js2-mode ()
  (require 'espresso)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4)
  (setq c-current-comment-prefix
        (if (listp c-comment-prefix-regexp)
            (cdr-safe (or (assoc major-mode c-comment-prefix-regexp)
                          (assoc 'other c-comment-prefix-regexp)))
          c-comment-prefix-regexp))
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'custom-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)]
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'custom-js2-indent)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode)))

(add-hook 'js2-mode-hook 'customize-js2-mode)

(defun customize-lisp-mode ()
  (setq indent-tabs-mode nil)
  (setq tab-width 2))

(add-hook 'lisp-mode-hook 'customize-lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'customize-lisp-mode)

;; Move lines or regions up/down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-line-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun move-line-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(global-set-key "\M-p" 'move-line-up)
(global-set-key "\M-n" 'move-line-down)
(put 'downcase-region 'disabled nil)
