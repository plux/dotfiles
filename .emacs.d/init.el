;;; init.el -- My emacs config
;; Moved to early-init.el
;; Adjust gc-cons-threshold. The default setting is too low.
;; The default is 800 kilobytes.  Measured in bytes.
;(setq gc-cons-threshold (* 100 1000 1000))
;(setq comp-deferred-compilation t)
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "%s [duration: %s] [gcs: %d]"
                     (emacs-version)
                     (format "%.2f s"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
            (setq gc-cons-threshold (* 100 1000 1000))))

;(require 'ccls)
;; Profile emacs startup
;; Stupid splash screen
;(setq inhibit-startup-message t)

;; Color theme
;(load-theme 'wombat)
;; Columns are nice
(column-number-mode 1)
(global-hl-line-mode)

;; Turn of menubar, toolbar and scrollbar
;(menu-bar-mode 0)
;(tool-bar-mode 0)
;(scroll-bar-mode 0)

(eval-when-compile
  (require 'use-package))

;(require 'diminish)
;(require 'bind-key)

(eval-when-compile
  (require 'lux-mode))
(use-package lux-mode
  :ensure t
  :bind
  ("C-c C-c" . lux-run-test)
  )

(defun lux-run-test ()
  "Run current lux test."
  (interactive ())
  (compile (format "source %senv.sh; make --keep-going LUX_FILES=%s"
                   (projectile-project-root)
                   (file-name-nondirectory (buffer-file-name)))))

(defun tailf-compile ()
  "Compile in tailf."
  (interactive ())
  (compile (format "source %senv.sh; make -j12"
                   (projectile-project-root))))

(defun tailf-eunit ()
  "Run eunit test in tailf."
  (interactive ())
  (compile (format "make -C ../test/eunit test-%s"
                   (erlang-get-module))))


(defun tailf-ct-suite ()
  "Run ct suite in tailf."
  (interactive ())
  (compile (format "make build test SUITES=\"%s\""
                   (erlang-get-module))))

(defun tailf-ct-case ()
  "Run ct testcase in tailf."
  (interactive ())
  (mark-defun)
  (if (and (erlang-get-module) (erlang-get-function-name))
      (compile (format "make build test SUITES=\"%s -case %s\""
                       (erlang-get-module)
                       (erlang-get-function-name))))
  (deactivate-mark))


(defun generate-compiledb ()
  "Generate compiledb"
  (interactive ())
  (compile (format "source %senv.sh; make clean; make all | compiledb -v"
                   (projectile-project-root))))
(setq c-basic-offset 4)

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)

;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(use-package corfu-terminal
  :ensure t
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  )

(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;; Disabled since icons get the wrong size..

;; (use-package all-the-icons-completion
;;   :ensure t
;;   :after (marginalia all-the-icons)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init
;;   (all-the-icons-completion-mode))

;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; (use-package corfu
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto nil)                 ;; Enable auto completion
;;   (corfu-separator ?\s)          ;; Orderless field separator
;;   (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   (corfu-quit-no-match t)      ;; Never quit, even if there is no match
;;   (corfu-preview-current t)    ;; Disable current candidate preview
;;   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   (corfu-echo-documentation nil) ;; Disable documentation in the echo area

;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin
;;   ;; Enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; See also `corfu-excluded-modes'.
;;   ;;  :hook ((lsp-mode . corfu-doc-mode))
;;   :init
;;   (global-corfu-mode))

;; ;; Use Dabbrev with Corfu!
;; (use-package dabbrev
;;   ;; Swap M-/ and C-M-/
;;   :bind (("M-/" . dabbrev-completion)
;;          ("C-M-/" . dabbrev-expand))
;;   ;; Other useful Dabbrev configurations.
;;   :custom
;;   (dabbrev-expand)
;;   (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; ;; Add extensions
;; (use-package cape
;;   :init
;;   ;; Add `completion-at-point-functions', used by `completion-at-point'.
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-history)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-tex)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-dict)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-line)
;;   )

;; Needed to enable corfu, otherwise conflict with company-mode
;; (setq lsp-completion-provider :none)
;; (defun corfu-lsp-setup ()
;;   (setq-local completion-styles '(orderless)
;;               completion-category-defaults nil))
;; (add-hook 'lsp-mode-hook #'corfu-lsp-setup)

(use-package swiper
  :ensure t
  )
(use-package corfu
  :ensure t
  :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu

  (corfu-auto 't)
  (corfu-auto-prefix 4)
  (corfu-auto-delay 0.25)
  ;; (corfu-min-width 30)
  ;; (corfu-max-width corfu-min-width)     ; Always have the same width
  ;;(corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)

  ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
  ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
  ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
  ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
  ;; configuration already has pre-prepared). Necessary for manual corfu usage with
  ;; orderless, otherwise first component is ignored, unless `corfu-separator'
  ;; is inserted.
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)            ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  ;; (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one
  (corfu-preview-current nil)
  ;(corfu-preselect-first t)        ; Preselect first candidate?

  ;; Other
  (corfu-echo-documentation nil)        ; Already use corfu-doc
  (lsp-completion-provider :none)       ; Use corfu instead for lsp completions
  :init
  (global-corfu-mode)
  :config
  ;; Setup lsp to use corfu for lsp completion
  (defun kb/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

;; (use-package corfu-doc
;;   :after corfu
;;   :hook (corfu-mode . corfu-doc-mode)
;;   :custom
;;   (corfu-doc-delay 0.5)
;;   (corfu-doc-max-width 70)
;;   (corfu-doc-max-height 20)

;;   ;; NOTE 2022-02-05: I've also set this in the `corfu' use-package to be
;;   ;; extra-safe that this is set when corfu-doc is loaded. I do not want
;;   ;; documentation shown in both the echo area and in the `corfu-doc' popup.
;;   (corfu-echo-documentation nil))


(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 65)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-major-mode-color-icon nil)
  ;; (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  ;; (doom-modeline-buffer-state-icon t)
  ;; (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  ;; (doom-modeline-enable-word-count nil)
  ;; (doom-modeline-buffer-encoding t)
  ;f; (doom-modeline-indent-info nil)
  ;; (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 30)
  ;;(doom-modeline-env-version t)
  ; (doom-modeline-irc-stylize 'identity)
  ;; (doom-modeline-github-timer nil)
  ;; (doom-modeline-gnus-timer nil)
  )

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Start emacs server
(load "server")
(unless (server-running-p) (server-start))
;; Global emacs settings
(use-package emacs
  :config
  (setq compilation-scroll-output t)
  ;; Use y/n instead of yes/no for questions
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Turn on global font lock mode
  (global-font-lock-mode 1)
  ;; This will prompt if opening a file with too long lines
  (global-so-long-mode 1)
  ;; Turn on hilighting of brackets
  (show-paren-mode t)
  ;; Parens
  (electric-pair-mode t)
  ;; Recentf
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)
  ;; View mode
  (setq view-read-only t)
  ;; Store backups
  (setq backup-directory-alist
        `((".*" . "~/.emacs-backup/")))
  (setq auto-save-file-name-transforms
        `((".*" "~/.emacs-backup/" t)))
  :bind
  ( ;("M-g"    . goto-line)
    ([S-down] . scroll-one-line-up)
    ([S-up]   . scroll-one-line-down)
    ("C-c a"  . align-regexp)
    ("C-c g"  . magit-status)
    ("C-s"    . swiper)
    ("C-c s"  . swiper-thing-at-point)
    ([f5]     . revert-buffer)
    ("C-x C-z" . selectrum-repeat)
    ("C-`"   . popper-toggle-latest)
    ("M-`"   . popper-cycle)
    ("C-M-`" . popper-toggle-type)
    ))

(defun run-projectile-invalidate-cache (&rest _args)
  ;; We ignore the args to `magit-checkout'.
  (projectile-invalidate-cache nil))

(advice-add 'magit-checkout
            :after #'run-projectile-invalidate-cache)
(advice-add 'magit-branch-and-checkout ; This is `b c'.
            :after #'run-projectile-invalidate-cache)
;; Use hippie auto completion
(use-package hippie-expand
  :init
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-list
                                           try-expand-line))
  :bind
  ("C-'" . hippie-expand)
  )

;; (use-package leetcode
;;   :ensure t
;;   :init
;;   (setq leetcode-prefer-language "python3")
;;   (setq leetcode-prefer-sql "mysql")
;;   (setq leetcode-save-solutions t)
;;   (setq leetcode-directory "~/leetcode")  
;;   )

;; Use meta + arrowkeys to switch windows
(use-package windmove
  :config
  (windmove-default-keybindings 'meta)
  )

(exec-path)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  ;; Add yang flycheck
  (setq exec-path (cons "/home/hakan/tailf/trunk/bin" exec-path))
  (flycheck-define-checker yang
    "A yang syntax checker using yanger."

    :command ("yanger" source)

    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": " (message) line-end)
     (error line-start (file-name) ":" line ": " (message) line-end))
    :modes yang-mode)
  (flycheck-add-mode 'yang 'yang-mode)
  (setq flycheck-checkers (cons 'yang flycheck-checkers))
  (setq flycheck-navigation-minimum-level #'warning)
  (setq flycheck-error-list-minimum-level #'warning)

  ;; Add lux flycheck
  (setq exec-path (cons "/home/hakan/tailf/trunk/system/test/bin" exec-path))
  (setenv "TEST_DIR" "/home/hakan/tailf/trunk/system/test")
  (flycheck-define-checker lux
    "A lux syntax checker using lux --mode validate."
    :command ("lux" "--mode" "validate" source-inplace)
    :error-patterns
    ((error line-start "\t" (file-name) ":" line ":" column " - " (message) line-end)
     (error line-start "\t" (file-name) ":" line " - " (message) line-end)
     )
    :modes lux-mode
    )
  (flycheck-add-mode 'lux 'lux-mode)
  (setq flycheck-checkers (cons 'lux flycheck-checkers))

  ;; Enable flycheck globally
  (global-flycheck-mode t)
  :bind
  ( ("C-c C-p" . flycheck-previous-error)
    ("C-c C-n" . flycheck-next-error)
    ("C-c C-e" . flycheck-first-error)
    )
  )

;(use-package flycheck-color-mode-line
;  :hook
;  (flycheck-mode . flycheck-color-mode-line-mode)
;  )


;; Undo/Redo for window management (undo = C-c left, redo = C-c right)
(use-package winner
  :config
  (winner-mode 1)
  :bind ( ("C-c u" . winner-undo)
          ("C-c r" . winnder-redo))
  )

;; Scrolling keybindings
(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or ARG lines)."
  (interactive)
  (scroll-up (or arg 1)))
(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or ARG lines)."
  (interactive)
  (scroll-down (or arg 1)))

;; Ace jump (TODO: replace with avy)
(use-package ace-jump-mode
  :disabled
  :bind
  (("C-j" . 'ace-jump-word-mode)
   ("M-j" . 'ace-jump-line-mode))
  )

;; Use whitespace mode to show whitespace
(use-package whitespace
  :init
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  :config
  (global-whitespace-mode t)
  )

;; Popper mode
;; (use-package popper
;;   :disabled t
;;   :init
;;   (setq popper-group-function #'popper-group-by-projectile)
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "\\*compilation\\*"
;;           "\\*Compile-Log\\*"
;;           "\\*Occur\\*"
;;           "^magit:"
;;           "\\*Backtrace\\*"
;;           "Output\\*$"
;;           magit-mode
;;           help-mode
;;           compilation-mode))
;;   (popper-mode +1))

;; Marginalia
(use-package marginalia
  :ensure t

  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (setq marginalia-command-categories
        (append '((projectile-find-file . project-file)
                  (projectile-find-dir . project-file)
                  (projectile-switch-project . file))
                marginalia-command-categories))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))
;; Orderless
(use-package orderless
  :ensure t
  :defer t
  :init
  (setq completion-styles '(orderless basic))
  )

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ;; ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ;; ("C-c b" . consult-bookmark)
         ;; ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ;; ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g s" . consult-lsp-symbols)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g q" . lsp-execute-code-action)
         ;; ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ;; ("M-s f" . consult-find)
         ;; ("M-s L" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ("C-c C-f" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
         ;; ("M-s m" . consult-multi-occur)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; ;; Isearch integration
         ;; ("M-s e" . consult-isearch)
         ;; :map isearch-mode-map
         ;; ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ;; ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ;; ("M-s l" . consult-line)                 ;; required by consult-line to detect isearch
         )
  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Selectrum, Vertico etc.
;  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
;  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. Note that the preview-key can also be
  ;; configured on a per-command basis via `consult-config'. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
;  (dolist (cmd '(consult-ripgrep consult-grep consult-bookmark consult-recent-file))
;    (setf (alist-get cmd consult-config) `(:preview-key ,(kbd "M-."))))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
;  (setq consult-project-root-function
;        (lambda ()
;          (when-let (project (project-current))
;            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

;; ;; Selectrum
;; (use-package selectrum
;;   :init
;;   :config
;;   (selectrum-mode +1)
;;   (setq orderless-skip-highlighting (lambda () selectrum-is-active))
;;   (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
;;   )

(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-prompter 'embark-completing-read-prompter)
  (setq embark-indicators '(embark-highlight-indicator
                            embark-isearch-highlight-indicator))
  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;; Helm
;; (use-package helm
;;   :ensure t
;;   :init
;;   (setq helm-buffer-details-flag nil)
;;   (setq helm-allow-mouse t)
;;   :config
;;   (helm-popup-tip-mode t)
;;   :bind (
;; ;;        ("M-x"     . helm-M-x)
;; ;;        ("C-x C-f" . helm-find-files)
;; ;;        ("C-x b"   . helm-mini)
;; ;;        ("C-c C-f" . helm-do-ag-project-root)
;;           ("C-c C-r" . helm-rg)
;;           ("C-c C-g" . helm-ag)
;;           ("C-c C-y" . helm-show-kill-ring)
;;           )
;;   )

;; Use projectile for project managment
(use-package projectile
  :ensure t
  :init
;  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
;  (helm-projectile-on)
  :diminish " p"
  )

(use-package eldoc
  :diminish "")

;; EditorConfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
  :diminish "")

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; Install the official Erlang mode
(use-package erlang
  :ensure t
  :defer t
  :init
  (setq erlang-check-module-name nil)
  :mode (("\\.erl?$" . erlang-mode)
         ("rebar\\.config$" . erlang-mode)
         ("relx\\.config$" . erlang-mode)
         ("sys\\.config\\.src$" . erlang-mode)
         ("sys\\.config$" . erlang-mode)
         ("\\.config\\.src?$" . erlang-mode)
         ("\\.config\\.script?$" . erlang-mode)
         ("\\.hrl?$" . erlang-mode)
         ("\\.app?$" . erlang-mode)
         ("\\.app.src?$" . erlang-mode)
         ("Emakefile" . erlang-mode)
         ("\\.lux$" . lux-mode)
         )
  :bind
  (("C-c C-f" . consult-ripgrep-erl)
   ("C-c C-t C-t" . tailf-ct-case)
   ("C-c C-t C-c" . tailf-compile)
   ("C-c C-t C-s" . tailf-ct-suite)
   ("C-c C-t C-e" . tailf-eunit))
  )

(defun consult-ripgrep-erl ()
  "Search for erlang files"
  (interactive)
  (let ((consult-ripgrep-command "rg --null --line-buffered --color=ansi --max-columns=1000\
   --no-heading --type erlang --type c --line-number . -e ARG OPTS"))
    (consult-ripgrep)))

;; Include the Language Server Protocol Clients
(use-package lsp-mode
  :ensure t
  :init
  (setq exec-path (cons "/home/hakan/git/erlang_ls/_build/default/bin" exec-path))
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-log-io t)
  :hook ((erlang-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (zig-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references)
  :bind
  (("C-o"  . lsp-rename)
   ("M-o" . lsp-execute-code-action)
  ))

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-sideline-enable t)
 ; (setq lsp-ui-sideline-show-hover nil)
;  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-show-code-actions t)
  :commands lsp-ui-mode
  :hook ((lsp-mode . lsp-ui-mode))
  )

;; Which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode t)
  :diminish ""
  )

;; LSP performance tweaks
;; Increase the amount of data which Emacs reads from the
;; process. Again the emacs default is too low 4k considering that the
;; some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Yasnippet
(use-package yasnippet
  :ensure t
  :hook ((lsp-mode . yas-minor-mode))
  :config
  )

(add-to-list 'load-path "/home/hakan/.emacs.d/lisp")
(use-package chatgpt-arcana
;  :ensure t
  :defer t
  :init
  (setq chatgpt-arcana-api-key "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")
  )


;; Company
;; (use-package company
;;   :ensure t
;;   :config
;;   (global-company-mode t)
;;   (setq company-idle-delay 0.5)
;;   :diminish ""
;;   :bind
;;   (([C-tab] . company-complete)
;;    ([C-return] . company-complete)
;;   ))

;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode)
;;   :diminish ""
;;   )

;; Nyan mode
(use-package nyan-mode
  :if window-system
  :ensure t
  :config
  (nyan-mode)
  (nyan-start-animation)
  )

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'edit-server-start t))

;; Rebar3 stuff
(defun rebar3-ct-suite ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile (format "rebar3 ct --suite %s" (erlang-get-module)))))

(defun rebar3-ct-case ()
  (interactive)
  (erlang-beginning-of-function)
  (let ((default-directory (projectile-project-root)))
    (compile (format "rebar3 ct --suite %s --case %s"
                     (erlang-get-module)
                     (erlang-get-function-name)))))


(defun rebar3-eunit ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile (format "rebar3 eunit -v --module %s"
                     (erlang-get-module)))))

;; Don't use tabs for indentation
(setq-default indent-tabs-mode nil)

(defun diary-file ()
  (interactive)
  (let ((daily-name (format-time-string "diary_%Y-%m-%d.org"))
        (diary-files (reverse (directory-files "~/cisco/diary/" nil "^diary.*.org$")))
        )
    (find-file (expand-file-name (concat "~/cisco/diary/" daily-name)))
    (if (car diary-files)
        (if (equal (car diary-files) daily-name)
            (message "exists")
          (insert-file-contents (car diary-files))))))



;; Cool helper fun
(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

;; Urlencode/decode region
(defun hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   '("4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "b0334e8e314ea69f745eabbb5c1817a173f5e9715493d63b592a8dc9c19a4de6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "eb122e1df607ee9364c2dfb118ae4715a49f1a9e070b9d2eb033f1cefd50a908" "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "1c596673c1d111e95a404bd12f8dd446cbcd47eee885271e21ffc98c3ac386cb" "3e038e9133010baa92e17a2c57f87336e91e6e76139d8c38d7d55d3c59a15967" "682a1161ee456e2d715ba30be61697fdbce8c08e23c2c6a1943f155e3e52f701" "147a0b0fce798587628774ae804a18a73f121e7e5c5fdf3a874ba584fdbe131d" "4e96c6ca1ab443d9804bcb55104848b25bdfda5ae665adeb218db1af07e7979a" "e503f6b2f45ecc5c5e295d1b3d84bb484206c4badbf716847a2445facf9f7495" "fe2a620695413fe5dcd74e03f0383e577effd7bb59527aa4d86444108d861504" "2f57ee6507f30d3228cdddadd0150e7b2fd85dd7c818c2d6485888c7249c37e8" default))
 '(erlang-check-module-name nil)
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#151515")
 '(flycheck-check-syntax-automatically '(save))
 '(flycheck-color-mode-line-show-running t)
 '(flymake-fringe-indicator-position 'left-fringe)
 '(flymake-note-bitmap '(exclamation-mark compilation-info))
 '(highlight-indent-guides-delay 0)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive 'stack)
 '(hl-line-overlay-priority 500)
 '(lsp-erlang-server 'erlang-ls)
 '(lsp-file-watch-threshold 1000)
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style 'chamfer)
 '(nyan-mode nil)
 '(package-selected-packages
   '(dumb-jump-mode flymake-shellcheck magit transient consult-flycheck ctrlf cargo cargo-transient rg jinx list-packages-ext default-text-scale hippie-expand all-the-icons swiper 0xc 0x0 doom-modeline marginalia emacs-everywhere esup company-prescient prescient selectrum vertico cape kind-icon all-the-icons-completion org org-modern highlight-indent-guides corfu corfu-doc command-log-mode org-tree-slide git-gutter zig-mode ccls company-erlang outline-magic origami fold-dwim fold-this dired-sidebar wgrep-ag wgrep lux-mode direnv org-static-blog minions smart-mode-line powerline flycheck-color-mode-line popper mini-frame consult embark embark-consult orderless dap-mode rainbow-delimiters rust-mode diminish helm-xref eglot outline-toc company-box helm-swoop flycheck-pos-tip emojify flycheck-yang yang-mode dash soothe-theme spacemacs-theme color-theme-sanityinc-tomorrow flatland-theme gruvbox-theme swiper-helm edts py-autopep8 blacken protobuf-mode company-jedi flycheck erlang slime projectile-ripgrep ripgrep iedit deft undo-tree know-your-http-well deadgrep helm-rg dumb-jump pdf-tools string-inflection use-package lsp-mode ensime csv helm-projectile helm-ls-git helm-fuzzy-find ace-jump-buffer ace-jump-helm-line ac-helm helm-ag helm-git helm-themes helm-lobsters helm-pass apib-mode ht dash-functional org-journal yaml-mode nyan-mode multiple-cursors markdown-preview-mode haskell-mode go-mode forecast flymd flycheck-rust eproject elpy elm-mode editorconfig edit-server dockerfile-mode cider autotetris-mode ansible ag ace-jump-mode winner whitespace helm projectile lsp-ui which-key yasnippet helm-lsp benchmark-init exec-path-from-shell))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#282828"))
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
 '(powerline-default-separator 'arrow)
 '(safe-local-variable-values '((vim . sw=2) (allout-layout . t)))
 '(set-fringe-style (nil . 0))
 '(sml/shorten-directory t)
 '(sml/show-file-name t)
 '(swiper-goto-start-of-match t)
 '(vc-follow-symlinks t)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 200 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(company-preview ((t (:foreground "#a0a0a0"))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t (:background "#494949"))) t)
 '(company-scrollbar-fg ((t (:background "#656565"))) t)
 '(company-tooltip ((t (:background "#494949" :foreground "white"))))
 '(company-tooltip-annotation ((t (:foreground "#cae682"))))
 '(company-tooltip-common ((t (:underline "#cae682"))))
 '(company-tooltip-search-selection ((t (:inherit highlight))))
 '(company-tooltip-selection ((t (:background "#656565" :foreground "white"))))
 '(edts-face-error-line ((t (:underline "#ff0000"))))
 '(edts-face-error-mode-line ((t (:box (:line-width 1 :color "red")))))
 '(edts-face-failed-test-line ((t (:underline "#ff0000"))))
 '(edts-face-warning-mode-line ((t (:box (:line-width 1 :color "gold")))))
 '(flycheck-color-mode-line-error-face ((t (:box (:line-width 1 :color "red3")))))
 '(flycheck-color-mode-line-info-face ((t nil)))
 '(flycheck-color-mode-line-warning-face ((t (:box (:line-width 1 :color "DarkOrange1")))))
 '(flycheck-error ((t (:underline (:color "Red1" :style wave)))))
 '(flycheck-fringe-error ((t (:foreground "red1"))))
 '(flymake-error ((t (:underline "tomato"))))
 '(flymake-note ((t (:underline "#58a4ed"))))
 '(flymake-warning ((t (:underline "gold2"))))
 '(helm-buffer-directory ((t (:background "gray25" :foreground "white"))))
 '(helm-buffer-file ((t nil)))
 '(helm-candidate-number ((t (:foreground "#ffc125"))))
 '(helm-ff-directory ((t (:background "gray25" :foreground "white"))))
 '(helm-ff-file ((t nil)))
 '(helm-header-line-left-margin ((t (:background "yellow" :foreground "black"))))
 '(helm-match ((t (:foreground "#ffc125"))))
 '(helm-selection ((t (:background "gray25" :distant-foreground "black"))))
 '(helm-source-header ((t (:foreground "#cae682" :weight bold))))
 '(highlight ((t (:background "#454545" :foreground "#ffffff" :underline nil))))
 '(hl-line ((t (:extend t :underline nil))))
 '(ivy-current-match ((t (:extend t :background "#454545"))))
 '(ivy-minibuffer-match-face-2 ((t (:underline t :weight ultra-bold))))
 '(lsp-face-highlight-textual ((t (:weight bold))))
 '(match ((t (:background "brown" :foreground "khaki"))))
 '(swiper-background-match-face-2 ((t (:inherit swiper-match-face-2))))
 '(swiper-line-face ((t (:inherit nil :background "#454545"))))
 '(swiper-match-face-2 ((t (:inverse-video t))))
 '(whitespace-line ((t (:background "gray9")))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
