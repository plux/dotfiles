;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(require 'benchmark-init)

;(use-package yasnippet
;  :hook (prog-mode . yas-minor-mode))

;(use-package yasnippet
;  :after (yasnippet))

;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;(use-package magit)
;(use-package magithub
;  :after magit
;;  :config
;;  (magithub-feature-autoinject t)
;;  (setq magithub-clone-default-directory "~/dev"))
;
;; Stupid splash screen
(setq inhibit-startup-message t)

;; Start emacs server
(load "server")
(unless (server-running-p) (server-start))

;; Color theme
(load-theme 'wombat)

;; Columns are nice
(column-number-mode 1)

;; Turn of menubar, toolbar and scrollbar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Use y/n instead of yes/no for questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Turn on global font lock mode
(global-font-lock-mode 1)

;; Turn on hilighting of brackets
(show-paren-mode t)

;; Which key mode
(which-key-mode t)


;(require 'auto-highlight-symbol)
;(global-auto-highlight-symbol-mode t)

;; Undo/Redo for window management (undo = C-c left, redo = C-c right)
(winner-mode 1)

(global-set-key "\C-cu" 'winner-undo)
(global-set-key "\C-cr" 'winner-redo)
;(setq lsp-keymap-prefix "C-l")

;; Scrolling keybindings
(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive)
  (scroll-up (or arg 1)))
(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or N)."
  (interactive)
  (scroll-down (or arg 1)))

;; Ido
;(ido-mode 1)

;; Parens
(electric-pair-mode t)

;; Smex config
;(global-set-key (kbd "M-x") 'smex)

;; Revert buffer
(global-set-key [f5] 'revert-buffer)

;; Goto line
;;(global-set-key "\C-l" 'goto-line)
;;(global-unset-key "\C-l")
(global-set-key "\M-g" 'goto-line)


;(defhydra hydra-my-compilation (global-map "M-g" :color red :columns 2)
;    "Compilation"
;    (";" previous-error "Previous error")
;    ("n" next-error "Next error")
;    ("l" recenter-top-bottom "recenter")
;    ("L" reposition-window "reposition")
;    ("0" first-error "First error")
;    ("q" nil "quit"))
;; Ace jump
(define-key global-map (kbd "C-j") 'ace-jump-word-mode)
(define-key global-map (kbd "M-j") 'ace-jump-line-mode)

;; Scrolling keybindings
(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive ";")
  (scroll-up (or arg 1)))
(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or N)."
  (interactive ";")
  (scroll-down (or arg 1)))

(global-set-key [S-down] 'scroll-one-line-up)
(global-set-key [S-up]  'scroll-one-line-down)
(global-set-key "\C-ca" 'align-regexp)
(global-set-key "\C-cg" 'magit-status)
(global-set-key "\C-x\M-g" 'magit-dispatch-popup)

;; key binding for auto complete
(global-set-key (kbd "C-'") 'hippie-expand)

;; auto-complete settings
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-list
                                         try-expand-line))

;; arrange for effective window-switching
(global-set-key [M-up]    'windmove-up)
(global-set-key [M-down]  'windmove-down)
(global-set-key [M-left]  'windmove-left)
(global-set-key [M-right] 'windmove-right)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; Helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(helm-fuzzier-mode)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key "\C-x\C-f" 'helm-find-files)
(global-set-key "\C-xb" 'helm-mini)
(global-set-key "\C-c\C-f" 'helm-do-ag-project-root)
(global-set-key "\C-c\C-r" 'helm-rg)
(global-set-key "\C-c\C-g" 'helm-ag)
(global-set-key "\C-c\C-y" 'helm-show-kill-ring)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; Popwin
; dont really remember why i have this, commenting it out
;(require 'popwin)
;(popwin-mode 1)

;; Iedit
; no idea what this is , so cmmenting it out
;(require 'iedit)

;; For EDTS
;;(add-to-list 'load-path "/home/hakan/dev/edts")
(setq exec-path (cons "/home/hakan/install/erl-22.3/bin" exec-path))
(setq exec-path (cons "/home/hakan/.bin" exec-path))

;; Ensure your Emacs environment looks like your user's shell one

; Define a utility function which either installs a package (if it is
;; missing) or requires it (if it already installed).
(defun package-require (pkg &optional require-name)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg))
  (if require-name
      (require require-name)
    (require pkg)))

(package-require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;(setq exec-path (cons "/home/hakan/install/erl-22.3/bin" exec-path))
;(setq exec-path (cons "/home/hakan/bin" exec-path))
;; erlang ls
; (setq exec-path (cons "/home/hakan/dev/erlang_ls/_build/default/bin" exec-path))

;;
;; Install the yasnippet dependency

;; Install the official lsp-mode package
;;(package-require 'lsp-mode)
;; It is usually a good idea to install lsp-ui as well
;;(package-require 'lsp-ui)
;; The lsp-ui sideline can become a serious distraction, so you
;; may want to disable it
;;(setq lsp-ui-sideline-enable nil)
;; Ensure docs are visible
 ;(setq lsp-ui-doc-enable t)
;;(setq lsp-ui-doc-enable nil)
;;(setq lsp-enable-xref t)
;;(setq lsp-prefer-flymake nil)
;;(setq lsp-log-io t)

;;(define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references)

;; Enable LSP automatically for Erlang files
;;(add-hook 'erlang-mode-hook #'lsp)

;; Override the key bindings from erlang-mode to use LSP for completion
;;(eval-after-load 'erlang
;;  '(define-key erlang-mode-map (kbd "C-M-i") #'company-lsp))

;; make local instead..
(global-set-key "\C-c\C-p" 'flycheck-previous-error)
(global-set-key "\C-c\C-n" 'flycheck-next-error)
(global-set-key [C-tab] 'company-complete)
(global-set-key [C-return] 'company-complete)

;; Hydra
(require 'hydra)

(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

(defhydra hydra-projectile (:color teal
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))

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


(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1))

(defun my-erlang-align-equals-in-clause ()
  (interactive)
  (require 'erlang)
  (save-excursion
    (erlang-indent-clause)
    (erlang-mark-clause)
    (align-to-equals (region-beginning) (region
-end))))


(add-to-list 'load-path "/home/hakan/dev/edts")

;; For rust
(setq exec-path (cons "/home/hakan/.cargo/bin" exec-path))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; For scala
(setq exec-path (cons "/home/hakan/install/sbt/bin" exec-path))

(setq deft-auto-save-interval 5.0)

(defun dv/erlang-create-export-signature ()
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((fun-name (symbol-at-point)))
      (let ((arity (erlang-get-function-arity)))
        (let ((export (format "-export([%s/%d])." fun-name arity)))
          (kill-new export)
          (message (format "Copied \"%s\"" export)))))))



;; Don't use tabs for indentation
(setq-default indent-tabs-mode nil)

;; Complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)

;; IELM
(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

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

;(use-package elpy
;  :ensure t
;  :init
;  (elpy-enable))
;(setq elpy-rpc-python-command "python3")
;(setq elpy-rpc-backend "jedi")

;; Enable autopep8
;(require 'py-autopep8)
;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  (require 'edts-start)
  (edit-server-start)
  (nyan-mode t)
  (nyan-start-animation)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   '("1c596673c1d111e95a404bd12f8dd446cbcd47eee885271e21ffc98c3ac386cb" "3e038e9133010baa92e17a2c57f87336e91e6e76139d8c38d7d55d3c59a15967" "682a1161ee456e2d715ba30be61697fdbce8c08e23c2c6a1943f155e3e52f701" "147a0b0fce798587628774ae804a18a73f121e7e5c5fdf3a874ba584fdbe131d" "4e96c6ca1ab443d9804bcb55104848b25bdfda5ae665adeb218db1af07e7979a" "e503f6b2f45ecc5c5e295d1b3d84bb484206c4badbf716847a2445facf9f7495" "fe2a620695413fe5dcd74e03f0383e577effd7bb59527aa4d86444108d861504" "2f57ee6507f30d3228cdddadd0150e7b2fd85dd7c818c2d6485888c7249c37e8" default))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil)
 '(display-time-format nil)
 '(display-time-mode t)
 '(edts-inhibit-package-check t)
 '(fci-rule-color "#151515")
 '(helm-allow-mouse t)
 '(helm-buffer-details-flag nil)
 '(helm-mode t)
 '(helm-popup-tip-mode t)
 '(nyan-wavy-trail nil)
 '(package-selected-packages
   '(edts py-autopep8 blacken protobuf-mode company-jedi hydra flycheck helm-lsp erlang company exec-path-from-shell lsp-ui slime projectile-ripgrep ripgrep iedit deft undo-tree know-your-http-well deadgrep helm-rg dumb-jump pdf-tools string-inflection use-package company-lsp lsp-mode ensime jedi csv helm-projectile helm-ls-git helm restclient-helm helm-fuzzy-find helm-fuzzier ace-jump-buffer ace-jump-helm-line ac-helm helm-ag helm-git helm-themes helm-tramp helm-lobsters helm-pass password-store apib-mode ht dash-functional nginx-mode org-journal yaml-mode smyx-theme smex pg nyan-mode multiple-cursors mic-paren markdown-preview-mode magit haskell-mode go-mode github-issues forecast flymd flycheck-rust eproject elpy elm-mode editorconfig edit-server dockerfile-mode cider autotetris-mode ansible ag ace-jump-mode))
 '(safe-local-variable-values '((allout-layout . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(edts-face-error-line ((t (:underline "#ff0000"))))
 '(edts-face-error-mode-line ((t (:box (:line-width 1 :color "red")))))
 '(edts-face-failed-test-line ((t (:underline "#ff0000"))))
 '(edts-face-warning-mode-line ((t (:box (:line-width 1 :color "gold")))))
 '(helm-buffer-directory ((t (:background "gray25" :foreground "white"))))
 '(helm-buffer-file ((t nil)))
 '(helm-candidate-number ((t (:foreground "#ffc125"))))
 '(helm-ff-directory ((t (:background "gray25" :foreground "white"))))
 '(helm-ff-file ((t nil)))
 '(helm-header-line-left-margin ((t (:background "yellow" :foreground "black"))))
 '(helm-match ((t (:foreground "#ffc125"))))
 '(helm-selection ((t (:background "gray25" :distant-foreground "black"))))
 '(helm-source-header ((t (:foreground "#cae682" :weight bold)))))
(put 'downcase-region 'disabled nil)

(defun dv/erlang-create-export-signature (fun arity)
  (format "-export([%s/%d])." fun arity))

(defun dv/erlang-current-function-name-and-arity ()
  (save-excursion
    (if (not (eobp)) (forward-char 1))
    (and (erlang-beginning-of-clause)
         (cons (erlang-get-function-name) (erlang-get-function-arity)))))

(defun dv/append-export-signature (fun arity)
  (let ((signature (dv/erlang-create-export-signature fun arity))
        (last-export (dv/erlang-last-export)))
    (if last-export

        (save-excursion
          (end-of-buffer)

          (if (re-search-backward last-export nil t)
              (progn
                (end-of-line)
                (newline-and-indent)
                (insert signature))
            (message (format "Could not find %s" last-export))))
      (progn
        (kill-new signature)
        (message (format "Could not locate export section, copied %s" signature))))))

(defun dv/add-export-signature ()
  (interactive)
  (let ((fun-and-arity (dv/erlang-current-function-name-and-arity)))
    (cond ((erlang-function-exported-p (car fun-and-arity) (cdr fun-and-arity)) (message "Function already exported."))
          ((eq nil (car fun-and-arity)) (message "No function under point"))
          (t (dv/append-export-signature (car fun-and-arity) (cdr fun-and-arity))))))


(defun dv/erlang-last-export ()
  (let ((last-export (last (erlang-get-export))))
    (when last-export
      (let ((last-export-string
             (regexp-quote (format "-export([%s/%d])."
                                   (caar last-export)
                                   (cdar last-export)))))
        last-export-string))))
