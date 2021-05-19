;; Adjust gc-cons-threshold. The default setting is too low.
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "%s [duration: %s] [gcs: %d]"
                     (emacs-version)
                     (format "%.2f s"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Stupid splash screen
(setq inhibit-startup-message t)

;; Color theme
(load-theme 'wombat)

;; Columns are nice
(column-number-mode 1)

;; Turn of menubar, toolbar and scrollbar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

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
  ;; Use y/n instead of yes/no for questions
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Turn on global font lock mode
  (global-font-lock-mode 1)
  ;; Turn on hilighting of brackets
  (show-paren-mode t)
  ;; Parens
  (electric-pair-mode t)
  :bind
  ( ("M-g"    . goto-line)
    ([S-down] . scroll-one-line-up)
    ([S-up]   . scroll-one-line-down)
    ("C-c a"  . align-regexp)
    ("C-c g"  . magit-status)
    ("C-s"    . swiper)
    ("C-c s"  . swiper-thing-at-point)
    ([f5]     . revert-buffer)
    ))

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

;; Use meta + arrowkeys to switch windows
(use-package windmove
  :config
  (windmove-default-keybindings 'meta)
  )

;; Flycheck
(use-package flycheck
  :config
  ;; Always show diagnostics at the bottom, using 1/5 of the available space
  (add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.20)))
  :bind
  ( ("C-c C-p" . flycheck-previous-error)
    ("C-c C-n" . flycheck-next-error)
    ("C-c C-e" . flycheck-first-error)
   )
  )


;; Undo/Redo for window management (undo = C-c left, redo = C-c right)
(use-package winner
  :config
  (winner-mode 1)
  :bind ( ("C-c u" . winner-undo)
          ("C-c r" . winnder-redo))
  )

;; Scrolling keybindings
(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive)
  (scroll-up (or arg 1)))
(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or N)."
  (interactive)
  (scroll-down (or arg 1)))

;; Ace jump
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
  (diminish 'global-whitespace-mode)
  )

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-buffer-details-flag nil)
  (setq helm-allow-mouse t)
  :config
  (helm-popup-tip-mode t)
  :bind ( ("M-x"     . helm-M-x)
          ("C-x C-f" . helm-find-files)
          ("C-x b"   . helm-mini)
          ("C-c C-f" . helm-do-ag-project-root)
          ("C-c C-r" . helm-rg)
          ("C-c C-g" . helm-ag)
          ("C-c C-y" . helm-show-kill-ring)
          )
  )

;; Use projectile for project managment
(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (helm-projectile-on)
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

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )

;; Install the official Erlang mode
(use-package erlang
  :defer t
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
         ("\\Emakefile" . erlang-mode))
  )

;; Include the Language Server Protocol Clients
(use-package lsp-mode
  :ensure t
  :init
  ;; erlang ls
  (setq exec-path (cons "/home/hakan/git/erlang_ls/_build/default/bin" exec-path))
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-log-io t)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (erlang-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references)
  (define-key lsp-mode-map (kbd "C-o") #'helm-lsp-workspace-symbol)
  )

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-sideline-enable nil)
  :commands lsp-ui-mode
  :hook ((lsp-mode . lsp-ui-mode))
  )

(use-package helm-lsp
  :ensure t
  :hook (helm-mode . helm-lsp)
  :commands helm-lsp-workspace-symbol
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
  :hook (;(prog-mode . yas-minor-mode)
         (lsp-mode . yas-minor-mode))
  :config
  (diminish 'yas-minor-mode " y")
  )

;; Company
(use-package company
  :ensure t
  :config
  (global-company-mode t)
  :diminish ""
  :bind
  (([C-tab] . company-complete)
   ([C-return] . company-complete)
  ))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :diminish ""
  )

;; Nyan mode
(use-package nyan-mode
  :disabled
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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(company-idle-delay 0.5)
 '(custom-safe-themes
   '("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "b0334e8e314ea69f745eabbb5c1817a173f5e9715493d63b592a8dc9c19a4de6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "eb122e1df607ee9364c2dfb118ae4715a49f1a9e070b9d2eb033f1cefd50a908" "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "1c596673c1d111e95a404bd12f8dd446cbcd47eee885271e21ffc98c3ac386cb" "3e038e9133010baa92e17a2c57f87336e91e6e76139d8c38d7d55d3c59a15967" "682a1161ee456e2d715ba30be61697fdbce8c08e23c2c6a1943f155e3e52f701" "147a0b0fce798587628774ae804a18a73f121e7e5c5fdf3a874ba584fdbe131d" "4e96c6ca1ab443d9804bcb55104848b25bdfda5ae665adeb218db1af07e7979a" "e503f6b2f45ecc5c5e295d1b3d84bb484206c4badbf716847a2445facf9f7495" "fe2a620695413fe5dcd74e03f0383e577effd7bb59527aa4d86444108d861504" "2f57ee6507f30d3228cdddadd0150e7b2fd85dd7c818c2d6485888c7249c37e8" default))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil)
 '(display-time-format nil)
 '(display-time-mode t)
 '(fci-rule-color "#151515")
 '(flymake-fringe-indicator-position 'left-fringe)
 '(flymake-note-bitmap '(exclamation-mark compilation-info))
 '(package-selected-packages
   '(dap-mode rainbow-delimiters company-fuzzy rust-mode diminish helm-xref eglot outline-toc company-box helm-swoop flycheck-pos-tip emojify flycheck-yang yang-mode dash soothe-theme spacemacs-theme color-theme-sanityinc-tomorrow flatland-theme gruvbox-theme counsel swiper-helm edts py-autopep8 blacken protobuf-mode company-jedi flycheck erlang slime projectile-ripgrep ripgrep iedit deft undo-tree know-your-http-well deadgrep helm-rg dumb-jump pdf-tools string-inflection use-package company-lsp lsp-mode ensime csv helm-projectile helm-ls-git helm-fuzzy-find ace-jump-buffer ace-jump-helm-line ac-helm helm-ag helm-git helm-themes helm-lobsters helm-pass apib-mode ht dash-functional org-journal yaml-mode nyan-mode multiple-cursors markdown-preview-mode magit haskell-mode go-mode forecast flymd flycheck-rust eproject elpy elm-mode editorconfig edit-server dockerfile-mode cider autotetris-mode ansible ag ace-jump-mode winner whitespace helm projectile lsp-ui which-key yasnippet company helm-lsp benchmark-init exec-path-from-shell))
 '(safe-local-variable-values '((vim . sw=2) (allout-layout . t)))
 '(swiper-goto-start-of-match t)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "#a0a0a0"))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t (:background "#494949"))))
 '(company-scrollbar-fg ((t (:background "#656565"))))
 '(company-tooltip ((t (:background "#494949" :foreground "white"))))
 '(company-tooltip-annotation ((t (:foreground "#cae682"))))
 '(company-tooltip-common ((t (:underline "#cae682"))))
 '(company-tooltip-search-selection ((t (:inherit highlight))))
 '(company-tooltip-selection ((t (:background "#656565" :foreground "white"))))
 '(edts-face-error-line ((t (:underline "#ff0000"))))
 '(edts-face-error-mode-line ((t (:box (:line-width 1 :color "red")))))
 '(edts-face-failed-test-line ((t (:underline "#ff0000"))))
 '(edts-face-warning-mode-line ((t (:box (:line-width 1 :color "gold")))))
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
 '(ivy-current-match ((t (:extend t :background "#454545"))))
 '(ivy-minibuffer-match-face-2 ((t (:underline t :weight ultra-bold))))
 '(lsp-face-highlight-textual ((t (:weight bold))))
 '(swiper-background-match-face-2 ((t (:inherit swiper-match-face-2))))
 '(swiper-line-face ((t (:inherit nil :background "#454545"))))
 '(swiper-match-face-2 ((t (:inverse-video t))))
 '(whitespace-line ((t (:background "gray9")))))

(put 'downcase-region 'disabled nil)
