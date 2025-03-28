;;; init.el -- My emacs config
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "%s [duration: %s] [gcs: %d]"
                     (emacs-version)
                     (format "%.2f s"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
            (setq gc-cons-threshold (* 100 1000 1000))))

;; Columns are nice
(column-number-mode 1)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/ultra-scroll")
(add-to-list 'load-path "~/.emacs.d/lisp/pgmacs")

(xterm-mouse-mode 1)

(require 'pgmacs)

(use-package ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

;; (use-package flycheck-overlay
;;   :init
;;   (setq flycheck-overlay-info-icon "i")
;;   (setq flycheck-overlay-warning-icon "⚠")
;;   (setq flycheck-overlay-error-icon "✘")
;;   :hook (flycheck-mode . flycheck-overlay-mode)
;; ;;  (add-hook 'flycheck-mode-hook #'flycheck-overlay-mode)
;;   )

(use-package kubed
  :ensure t
  :init
  (keymap-global-set "C-c k" 'kubed-prefix-map)
  )



;; Make compilation buffer handle ansi colors
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line)
  )



(use-package
  diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  )

(eval-when-compile
  (require 'use-package))

(defun sort-words-in-region (start end)
  "Sort words in the selected region alphabetically."
  (interactive "r")
  (save-excursion
    (let ((words (split-string (buffer-substring-no-properties start end))))
      (delete-region start end)
      (insert (mapconcat 'identity (sort words 'string<) " ")))))

;; Hacks for Templ, Tailwind, htmx 
;; Stolen from https://drshapeless.com/blog/posts/writing-go-templ-with-emacs.html

(defun drsl/is-class-attr ()
  (let ((mynode (treesit-node-parent
                 (treesit-node-parent
                  (treesit-node-at (point))))))
    (and (string= (treesit-node-type mynode) "attribute")
         (string= (treesit-node-text (treesit-node-child mynode 0))
                  "class"))))

(defun drsl/bounds-of-keyword ()
  (if (or (char-equal (char-before) ?\s)
          (char-equal (char-before) ?\"))
      nil
    (let ((START (save-excursion
                   (re-search-backward "[\s\"\t\n]"
                                       (line-beginning-position)
                                       t)))
          (END (save-excursion
                 (re-search-forward "[\s\"\t\n]"
                                    (line-end-position)
                                    t))))
      (cons (if START
                (1+ START)
              (line-beginning-position))
            (if END
                (1- END)
              (point))))))

(defcustom drsl/tailwind-css-keyword-file
  (expand-file-name "dict/tailwind_css_keyword.txt" user-emacs-directory)
  "tailwindcss keyword file path."
  :type 'string)

(defun drsl/tailwind-css-dict-list (input)
  "Return all words from `drsl/tailwind-css-keyword-file' matching INPUT."
  (unless (equal input "")
    (let* ((inhibit-message t)
           (message-log-max nil)
           (default-directory
            (if (and (not (file-remote-p default-directory))
                     (file-directory-p default-directory))
                default-directory
              user-emacs-directory))
           (files (mapcar #'expand-file-name
                          (ensure-list
                           drsl/tailwind-css-keyword-file)))
           (words
            (apply #'process-lines-ignore-status
                   "grep"
                   (concat "-Fh"
                           (and (cape--case-fold-p cape-dict-case-fold) "i")
                           (and cape-dict-limit (format "m%d" cape-dict-limit)))
                   input files)))
      (cons
       (apply-partially
        (if (and cape-dict-limit (length= words cape-dict-limit))
            #'equal #'string-search)
        input)
       (cape--case-replace-list cape-dict-case-replace input words)))))

(defun drsl/templ-tailwind-cape-dict ()
  (when (drsl/is-class-attr)
    (pcase-let ((`(,beg . ,end) (drsl/bounds-of-keyword)))
      `(,beg ,end
             ,(cape--properties-table
               (completion-table-case-fold
                (cape--dynamic-table beg end #'drsl/tailwind-css-dict-list)
                (not (cape--case-fold-p cape-dict-case-fold)))
               :sort nil ;; Presorted word list (by frequency)
               :category 'cape-dict)
             ,@cape--dict-properties))))

(defvar drsl/templ-ts-mode-tag-list
  '("a" "abbr" "address" "area" "article" "aside" "audio" "b"
    "base" "bdi" "bdo" "blockquote" "body" "br" "button" "canvas"
    "caption" "cite" "code" "col" "colgroup" "data" "datalist"
    "dd" "del" "details" "dfn" "dialog" "div" "dl" "dt" "em"
    "embed" "fieldset" "figcaption" "figure" "footer" "form" "h1"
    "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html"
    "i" "iframe" "img" "input" "ins" "kbd" "label" "legend" "li"
    "link" "main" "map" "mark" "math" "menu" "meta" "meter" "nav"
    "noscript" "object" "ol" "optgroup" "option" "output" "p"
    "picture" "pre" "progress" "q" "rp" "rt" "ruby" "s" "samp"
    "script" "search" "section" "select" "slot" "small" "source"
    "span" "strong" "style" "sub" "summary" "sup" "svg" "table"
    "tbody" "td" "template" "textarea" "tfoot" "th" "thead" "time"
    "title" "tr" "track" "u" "ul" "var" "video" "wbr")
  "HTML tags used for completion.

Steal from `web-mode'.")

(defvar drsl/templ-ts-mode-attribute-list
  '("accept" "accesskey" "action" "alt" "async" "autocomplete" "autofocus"
    "autoplay" "charset" "checked" "cite" "class" "cols" "colspan" "content"
    "contenteditable" "controls" "coords" "data" "datetime" "default" "defer"
    "dir" "dirname" "disabled" "download" "draggable" "enctype" "for" "form"
    "formaction" "headers" "height" "hidden" "high" "href" "hreflang" "http"
    "id" "ismap" "kind" "label" "lang" "list" "loop" "low" "max" "maxlength"
    "media" "method" "min" "multiple" "muted" "name" "novalidate" "onabort"
    "onafterprint" "onbeforeprint" "onbeforeunload" "onblur" "oncanplay"
    "oncanplaythrough" "onchange" "onclick" "oncontextmenu" "oncopy"
    "oncuechange" "oncut" "ondblclick" "ondrag" "ondragend" "ondragenter"
    "ondragleave" "ondragover" "ondragstart" "ondrop" "ondurationchange"
    "onemptied" "onended" "onerror" "onfocus" "onhashchange" "oninput"
    "oninvalid" "onkeydown" "onkeypress" "onkeyup" "onload" "onloadeddata"
    "onloadedmetadata" "onloadstart" "onmousedown" "onmousemove" "onmouseout"
    "onmouseover" "onmouseup" "onmousewheel" "onoffline" "ononline"
    "onpagehide" "onpageshow" "onpaste" "onpause" "onplay" "onplaying"
    "onpopstate" "onprogress" "onratechange" "onreset" "onresize" "onscroll"
    "onsearch" "onseeked" "onseeking" "onselect" "onstalled" "onstorage"
    "onsubmit" "onsuspend" "ontimeupdate" "ontoggle" "onunload"
    "onvolumechange" "onwaiting" "onwheel" "open" "optimum" "pattern"
    "placeholder" "poster" "preload" "readonly" "rel" "required" "reversed"
    "rows" "rowspan" "sandbox" "scope" "selected" "shape" "size" "sizes"
    "span" "spellcheck" "src" "srcdoc" "srclang" "srcset" "start" "step"
    "style" "tabindex" "target" "title" "translate" "type" "usemap" "value"
    "width" "wrap")
  "HTML attributes used for completion.

Steal from `web-mode'.")

(defun drsl/templ-ts-mode-completion ()
  "templ-ts-mode completion function.

The built-in treesit is required."
  (cond (;; completing tag name, e.g. <d
         (let ((bounds (or (bounds-of-thing-at-point 'word)
                           (cons (point) (point)))))
           (when (char-equal (char-before (car bounds)) ?\<)
             (list (car bounds)
                   (cdr bounds)
                   drsl/templ-ts-mode-tag-list
                   :annotation-function (lambda (_) " HTML Tag")
                   :company-kind (lambda (_) 'text)
                   :exclude 'no))))

        (;; completing attribute name, e.g. <div c
         (or (string= (treesit-node-type (treesit-node-at (point)))
                      "attribute_name")
             (string= (treesit-node-type (treesit-node-at (point)))
                      ">"))
         (let ((bounds (bounds-of-thing-at-point 'word)))
           (when bounds
             (list (car bounds)
                   (cdr bounds)
                   drsl/templ-ts-mode-attribute-list
                   :annotation-function (lambda (_) " HTML Attr")
                   :company-kind (lambda (_) 'text)
                   :exclusive 'no))))
        ))

(defvar drsl/htmx-attribute-list
  '("hx-get" "hx-post" "hx-on" "hx-push-url" "hx-select" "hx-select-oob"
    "hx-swap" "hx-swap-oob" "hx-target" "hx-trigger" "hx-vals" "hx-boost"
    "hx-confirm" "hx-delete" "hx-disable" "hx-disabled-elt" "hx-disinherit"
    "hx-encoding" "hx-ext" "hx-headers" "hx-history" "hx-history-elt"
    "hx-include" "hx-indicator" "hx-params" "hx-patch" "hx-preserve"
    "hx-prompt" "hx-put" "hx-replace-url" "hx-request" "hx-sync" "hx-validate")

  "Htmx attributes used for completion.")

(defvar drsl/htmx-swap-keyword-list
  '("innerHTML" "outerHTML" "beforebegin" "afterbegin" "beforeend"
    "afterend" "delete" "none")
  "Keywords for hx-swap.")

(defvar drsl/htmx-target-keyword-list
  '("this" "closest" "find" "next" "previous")
  "Keywords for hx-target.")

(defun drsl/get-htmx-value-list (ATTR)
  "Return a list of htmx value.

ATTR is the attribute name.
Only support hx-swap, hx-swap-oob, hx-target."
  (cond ((string-prefix-p "hx-swap" ATTR)
         drsl/htmx-swap-keyword-list)
        ((string= ATTR "hx-target")
         drsl/htmx-target-keyword-list)))

(defun drsl/templ-ts-mode-htmx-completion ()
  "templ-ts-mode completion for htmx.

Built-in treesit is required."
  (cond (;; completion of htmx attr name, e.g. <div hx-swap
         (or (string= (treesit-node-type (treesit-node-at (point)))
                      "attribute_name")
             (string= (treesit-node-type (treesit-node-at (point)))
                      ">"))
         ;; This mess if for the case when a - is typed.
         ;;
         ;; In the case of hx-swap*, where * is the pointer.
         ;;
         ;; Since word only includes swap, but symbol includes from
         ;; hx-swap... to the infinity, so just select the first of
         ;; symbol and last of word. But when a - is type, the
         ;; bounds of word returns nil, so just set it to the
         ;; `point'.
         ;;
         ;; TODO: This is an issue in the syntax table of
         ;; `templ-ts-mode'.
         ;;
         (let ((bounds (drsl/bounds-of-keyword)))
           (when bounds
             (list (car bounds)
                   (cdr bounds)
                   drsl/htmx-attribute-list
                   :annotation-function (lambda (_) " htmx attr")
                   :company-kind (lambda (_) 'text)
                   :exclusive 'no)))
         )
        (;; completion of some htmx value, e.g. <div hx-swap="innerHTML"
         (string= (treesit-node-type (treesit-node-parent
                                      (treesit-node-at (point))))
                  "quoted_attribute_value")
         (let ((words (drsl/get-htmx-value-list
                       (treesit-node-text
                        (treesit-node-prev-sibling
                         (treesit-node-parent (treesit-node-at (point)))
                         t)
                        t)))
               (bounds (or (bounds-of-thing-at-point 'word)
                           (cons (point) (point)))))
           (when words
             (list (car bounds)
                   (cdr bounds)
                   words
                   :annotation-function (lambda (_) " htmx value")
                   :company-kind (lambda (_) 'text)
                   :exclusive 'no)
             ))
         )))

;; (defun drsl/templ-ts-mode-insert-slash ()
;;   "Auto closing tag when inserting slash in `templ-ts-mode'"
;;   (interactive)
;;   (if (char-equal (char-before) ?\<)
;;       (let ((TAG (or (treesit-node-text
;;                       (treesit-node-child
;;                        (drsl/treesit-prev-sibling-until
;;                         (treesit-node-at (point))
;;                         (lambda (NODE)
;;                           (string= (treesit-node-type NODE)
;;                                    "tag_start")))
;;                        1)
;;                       t)

;;                      (when (drsl/treesit-next-sibling-until
;;                             (treesit-node-parent (treesit-node-at (point)))
;;                             (lambda (NODE)
;;                               (string= (treesit-node-type NODE)
;;                                        "tag_end")))
;;                        (treesit-node-text
;;                         (treesit-node-child
;;                          (drsl/treesit-prev-sibling-until
;;                           (treesit-node-parent (treesit-node-at (point)))
;;                           (lambda (NODE)
;;                             (string= (treesit-node-type NODE)
;;                                      "tag_start")))
;;                          1)
;;                         t))
;;                      )))
;;         (if TAG
;;             (progn (insert ?\/
;;                            TAG
;;                            ?\>)
;;                    (treesit-indent))
;;           (insert ?\/)))
;;     (insert ?\/)))

;; (keymap-unset templ-ts-mode-map "/" #'drsl/templ-ts-mode-insert-slash)

(add-hook 'templ-ts-mode-hook
          (lambda ()
            (progn
              (add-to-list 'drsl/eglot-extra-completion-functions
                           #'drsl/templ-tailwind-cape-dict)
              (add-to-list 'drsl/eglot-extra-completion-functions
                           #'drsl/templ-ts-mode-completion)
              (add-to-list 'drsl/eglot-extra-completion-functions
                           #'drsl/templ-ts-mode-htmx-completion))))

(defcustom drsl/eglot-extra-completion-functions '(cape-file)
  "extra completion functions for eglot"
  :type '(repeat string))

(defun drsl/eglot-capf ()
  (mapc
   (lambda (FUNCTION)
     (add-to-list 'completion-at-point-functions
                  FUNCTION))
   drsl/eglot-extra-completion-functions))

(add-hook 'eglot-managed-mode-hook #'drsl/eglot-capf)


(use-package emacs-everywhere)
(require 'yang-mode nil t)

(eval-when-compile
  (require 'lux-mode))

(use-package lux-mode
  :ensure t
  :bind
  ("C-c C-c" . lux-run-test)
  )

(use-package magit
  :ensure t
  :init
    (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
    (setq magit-bury-buffer-function 'magit-restore-window-configuration)
    )

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; dired stuff
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-verbosity nil)
(setq dired-listing-switches "-alFh")



(add-to-list 'load-path "~/git/emacs-run-command-recipes")
(require 'run-command-recipes)
(use-package run-command-recipes
  :ensure nil
  :after (run-command)
  :init (run-command-recipes-use-all))

(use-package run-command
  :ensure t
  :init
;;  (setq run-command-default-runner 'run-command-runner-compile)
  :bind
  ("C-x C-x" . run-command))


(setq display-buffer-alist
      '(("\\*projterm.*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.3)
         (dedicated . t)
         )))

;; (defun toggle-my-projectile-vterm ()
;;   "Toggle vterm in the project's root."
;;   (interactive)
;;   ()
;;   (if (s-starts-with? "*projterm" (buffer-name))
;;       (close-my-projectile-vterm-window))
;;     (open-my-projectile-vterm nil 't))


(defun toggle-my-projectile-vterm ()
  "Toggle vterm in the project's root."
  (interactive)
  ()
  (let* ((project (projectile-acquire-root))
         (buffer (projectile-generate-process-name "projterm" nil project)))
    (if (get-buffer-window buffer)
        (delete-window (get-buffer-window buffer))
      (open-my-projectile-vterm nil 't))))

(defun close-my-projectile-vterm-window ()
  "Close vterm in the project's root."
  (interactive)
  (let* ((project (projectile-acquire-root))
         (buffer (projectile-generate-process-name "projterm" nil project)))
    (delete-window (get-buffer-window buffer))))

;; (global-set-key (kbd "C-q") 'toggle-my-projectile-vterm)
(global-set-key (kbd "C-c q") 'toggle-my-projectile-vterm)

(defun open-my-projectile-vterm (&optional new-process other-window)
  "Invoke `vterm' in the project's root.

Use argument NEW-PROCESS to indicate creation of a new process instead.
Use argument OTHER-WINDOW to indentation whether the buffer should
be displayed in a different window.

Switch to the project specific term buffer if it already exists."
  (interactive "P")
    (let* ((project (projectile-acquire-root))
         (buffer (projectile-generate-process-name "projterm" new-process project)))
    (unless (require 'vterm nil 'noerror)
      (error "Package 'vterm' is not available"))
    (if (buffer-live-p (get-buffer buffer))
        (if other-window
            (switch-to-buffer-other-window buffer)
          (switch-to-buffer buffer))
      (projectile-with-default-dir project
        (if other-window
            (vterm-other-window buffer)
          (vterm buffer))))))

;; (setq display-buffer-alist
;;       '(
;;         ("\\*vterm"
;;          (display-buffer-reuse-mode-window display-buffer-at-bottom)
;;          (dedicated . t)
;;          )))

;;                                         ;(setq switch-to-buffer-obey-display-actions t)

;; (setq switch-to-buffer-in-dedicated-window 'pop)

(use-package dired-subtree
        :ensure t
        :bind (:map dired-mode-map
                    ("<tab>" . dired-subtree-toggle)))


(defun launch-terminal ()
  "Launch a external terminal."
  (interactive ())
  (shell-command "gnome-terminal"))


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
  (compile (format "source %senv.sh; make %s test-%s"
                   (projectile-project-root)
                   (if (s-ends-with? "/eunit/" (file-name-parent-directory (buffer-file-name)))
                       ""
                       "-C ../test/eunit")
                   (erlang-get-module))))

;; (defun tailf-format ()
;;   "Run `make erlfmt_format' in tailf."
;;   (interactive ())
;;   (when (eq major-mode 'erlang-mode)
;;     (call-process-shell-command
;;      (format "make -C %s erlfmt_format" (projectile-project-root))
;;      nil "*erlfmt output*" t)))

;; (add-hook 'after-save-hook 'tailf-format)



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

(setq c-basic-offset 2)
(setq ring-bell-function 'ignore)

;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ))



(use-package corfu-terminal
;  :ensure t
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

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))


(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

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
  (corfu-auto t)
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
  :bind
  (("C-<tab>" . completion-at-point)
   )
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
  (doom-modeline-height 35)
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

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(defun only-if-use-region (func &rest args)
  (if (use-region-p)
      (apply func args)))

(advice-add 'electric-pair-post-self-insert-function :around 'only-if-use-region)

;; Start emacs server
(load "server")
(unless (server-running-p) (server-start))
;; Global emacs settings
(use-package emacs
  :config
  (setq confirm-kill-emacs 'y-or-n-p)
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
    ("C-`"   . popper-toggle-latest)
    ("M-`"   . popper-cycle)
    ("C-M-`" . popper-toggle-type)
    ("C-c RET" . new-terminal)
    ))


;; (defun run-projectile-invalidate-cache (&rest _args)
;;   ;; We ignore the args to `magit-checkout'.
;;   (projectile-invalidate-cache nil))

;; (advice-add 'magit-checkout
;;             :after #'run-projectile-invalidate-cache)
;; (advice-add 'magit-branch-and-checkout ; This is `b c'.
;;             :after #'run-projectile-invalidate-cache)
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

;; (use-package popwin
;;   :ensure t
;;   :config
;;   (popwin-mode 1)
;;   )

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
  (setq exec-path (cons "~/dev/tailf/trunk/bin" exec-path))
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
  (setq exec-path (cons "~/dev/tailf/trunk/system/test/bin" exec-path))
  (setenv "TEST_DIR" "~/dev/tailf/trunk/system/test")
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
    ("M-p" . flycheck-previous-error)
    ("M-n" . flycheck-next-error)
    )
  )

;; Undo/Redo for window management (undo = C-c left, redo = C-c right)
(use-package winner
  :config
  (winner-mode 1)
  :bind ( ("C-c u" . winner-undo)
          ("C-c r" . winner-redo))
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

(use-package avy
  :ensure t
  :bind
  (("C-j" . 'avy-goto-char)
   ("M-j" . 'avy-goto-char-timer))
  )

;; Use whitespace mode to show whitespace
(use-package whitespace
  :init
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  :config
  (global-whitespace-mode t)
  )

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
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; Other custom bindings
         ([remap yank-pop] . consult-yank-pop) ;; orig. yank-pop
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
         ("C-c i" . consult-imenu)
         ("C-c b" . consult-bookmark)

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


(use-package jq-mode
  :ensure t
  )
(use-package restclient
  :ensure t
  )

(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Don't grow and shrink the Vertico minibuffer
  (setq vertico-resize 'fixed)

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

(use-package sudo-edit
  :ensure t
  :after embark
  :bind
  (:map embark-file-map
        ("s" . sudo-edit-find-file))
  (:map embark-become-file+buffer-map
        ("s" . sudo-edit-find-file)))



;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Use projectile for project managment
(use-package projectile
  :ensure t
  :init
;  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching nil)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
;  (helm-projectile-on)
  :diminish " p"
  )

(use-package eldoc
  :ensure t
  :init
  (setq eldoc-echo-area-use-multiline-p t)
  )

(use-package eldoc-box
  :ensure t
  :bind
  ("C-h D" . eldoc-box-help-at-point))

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

(use-package indent-bars
  :ensure t
  :config
    (setq
    indent-bars-color '(highlight :face-bg t :blend 0.25)
    indent-bars-pattern "."
    indent-bars-width-frac 0.1
    indent-bars-pad-frac 0.1
    indent-bars-zigzag nil
    ;; indent-bars-color-by-depth nil
    indent-bars-highlight-current-depth nil
    ;; indent-bars-display-on-blank-lines nil
    )
  :hook ((yang-mode prog-mode) . indent-bars-mode))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)


(setq whitespace-style '(face empty tabs lines-tail trailing))

(use-package go-mode
  :ensure t
  :hook ((go-ts-mode . my-go-whitespace-style)
         (go-mode . my-go-whitespace-style)
         (before-save . gofmt-before-save))
  :config
  (setq go-ts-mode-indent-offset 2)

  (setq tab-width 2)
  (defun my-go-whitespace-style ()
    "Custom whitespace style for Go mode."
    (setq whitespace-style '(face empty lines-tail trailing)))
  :bind
  (("C-c C-c" . go-build)
   ("C-c C-r" . go-run)
   ("C-c C-t" . go-test)
   ))

(defun go-build ()
  "Go build."
  (interactive ())
  (compile "go build ."))

(defun go-run ()
  "Go run."
  (interactive ())
  (compile "go run ."))

(defun go-test ()
  "Go test."
  (interactive ())
  (compile "go test"))

;; (defun my-eglot-format ()
;;   "Format the buffer with eglot if erlang-mode."
;;   (when (eq major-mode 'erlang-mode)
;;     (eglot-format-buffer))
;;   )

;; (defun tailf-format ()
;;   "Format the buffer with eglot if erlang-mode.
;;    Run the command make erlfmt_format in the root of the project.
;;   "
;;   (interactive ())
;;   (when (eq major-mode 'erlang-mode)
;;     (compile "make -C %s erlfmt_format"))
;;   )

;; (add-hook 'after-save-hook 'tailf-format)

;; Install the official Erlang mode
(use-package erlang
  :ensure t
  :defer t
  :init
  (setq erlang-check-module-name nil)
  :hook
  (before-save . eglot-format-buffer)
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
  (
   :map erlang-mode-map
   ("C-c C-f" . consult-ripgrep)
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

;(use-package gptel :ensure t)
(add-to-list 'load-path "~/git/copilot.el")
(use-package copilot
    :init
    :config
    (setq copilot-indent-offset-warning-disable t)
    (setq copilot-max-char 1000000)
    :hook ((prog-mode . copilot-mode)
           (lux-mode . copilot-mode)
           (python-mode . copilot-mode)
           (yang-mode . copilot-mode))
    :bind
    (("C-c c" . copilot-complete)
     ("C-c y" . copilot-accept-completion-by-line)
     ("C-c Y" . copilot-accept-completion)
     ("C-c N" . copilot-next-completion)
     ("C-c P" . copilot-previous-completion)
     ))


;;(add-to-list 'load-path "~/git/copilot-chat.el")
(use-package request
 :ensure t)

(use-package copilot-chat
  :ensure t
   :after (request shell-maker)
   :custom
   (copilot-chat-frontend 'shell-maker)
   :config
   (require 'copilot-chat-shell-maker)
   (push '(shell-maker . copilot-chat-shell-maker-init) copilot-chat-frontend-list)
   (copilot-chat-shell-maker-init))

(use-package lsp-tailwindcss
  :ensure t
  :hook ((css-mode . lsp-tailwindcss-enable)
         (web-mode . lsp-tailwindcss-enable)
         (mhtml-mode . lsp-tailwindcss-enable))
  :after lsp-mode
  (setq lsp-tailwindcss-add-on-mode t)
  )

;; (use-package flycheck-eglot
;;   :ensure t
;;   :after (flycheck eglot)
;;   :config
;;   (global-flycheck-eglot-mode 1))

(use-package eglot
  :ensure t
  :init
  (setq-default eglot-workspace-configuration
                '((:gopls .
                          ((hints . ((parameterNames . t)
                                     ;;                                     (assignVariableTypes . t)
                                     (constantValues . t)
                                     ))
                           (staticcheck . t)
                           (usePlaceholders . t)
                           (completeFunctionCalls . t)
                           (diagnosticsTrigger . "Save")
;;                           (buildFlags . ["-tags e2e"])
                           (analyses . ((fillreturns . t)))
                           ;; (codelenses . ((generate . t)
                           ;;                (test . t)
                           ;;                ))
                           ))
                  ))
  ;; (setq-default eglot-workspace-configuration
  ;;               (quote (:gopls (:hints ((:parameterNames .t)))
;;  :assignVariableTypes
  ;;                        (:diagnosticsTrigger "Save")
  ;;                        (:usePlaceholders t)
  ;;                        ;;                               (buildFlags . ["-tags e2e"])
  ;;                        (:staticcheck t)
  ;;                        )))
  ;;(setenv "GOBUILD" "-tags e2e")
  (setq exec-path (cons "~/git/erlang_ls/_build/default/bin" exec-path))
  ;;(setq exec-path (cons "/home/hakan/git/erlang_ls/dev" exec-path))
  :hook ((erlang-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (templ-ts-mode . eglot-ensure)
         (web-mode . eglot-ensure))
  :bind
  (("C-o" . eglot-rename)
   ("M-o" . eglot-code-actions)
   ("C-c C-p" . flymake-goto-prev-error)
   ("C-c C-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)
   ("M-n" . flymake-goto-next-error)
   ("C-c C-e" . consult-flycheck)
   ("C-c e" . eglot)
   )
  )

;; Open files in other window
(setq server-window 'pop-to-buffer)

(defun vterm-send-C-k ()
  "Send `C-k' to libvterm."
  (interactive)
  (kill-ring-save (point) (vterm-end-of-line))
  (vterm-send-key "k" nil nil t))

(defun my-vterm-mode-customizations ()
  (local-set-key (kbd "C-k") 'vterm-send-C-k))

(add-hook 'vterm-mode-hook 'my-vterm-mode-customizations)

(defun get-current-dir-i ()
    (interactive)
    (message "%s" (get-current-dir)))

(defun extract-path (str)
  (if (string-match "\\*vterm\\* \\([^<]*\\)" str)
      (match-string 1 str)
    nil))

(defun get-current-dir ()
  (if (buffer-file-name)
      (file-name-directory (buffer-file-name))
    (or (extract-path (buffer-name))
        default-directory)))

(defun new-terminal ()
  (interactive)
  ;; This will split the window if there's only one window
  ;; Try to use current directory as default directory
  (open-multi-vterm-in-other-window (get-current-dir)))


(defun open-multi-vterm-in-other-window (dir)
  "Open `multi-vterm' in the other window. Split window if necessary.
If the current buffer name starts with `*vterm*`, split below."
  (if (string-prefix-p "*vterm*" (buffer-name))
      (progn
        (split-window-below) ; Split the window horizontally if in a vterm buffer
        (other-window 1))   ; Move to the window below
    (if (one-window-p)
        (split-window-right))     ; Split the window vertically if there's only one window
    (other-window 1))       ; Move to the other window in any case
  (let ((default-directory dir))
        (multi-vterm)))

(defun dedicate-window (&optional arg)
  "Set current window to be dedicated.
   With prefix ARG, undedicate it."
  (interactive "P")
  (set-window-dedicated-p (get-buffer-window (current-buffer)) (not arg))
  (message (if arg
               "Window '%s' is normal"
               "Window '%s' is dedicated")
           (current-buffer)))

(use-package vterm
  :ensure t
  :init
  ;;(setq vterm-buffer-name-string "*vterm* %s")
  (setq vterm-max-scrollback 100000)
  ; (setq vterm-enable-manipulate-selection-data-by-osc52 t)
  :bind
  (("C-c t" . vterm-toggle)
   ("C-c v" . hn/switch-to-vterm-buffer)
   :map vterm-mode-map
   ("C-q" . 'toggle-my-projectile-vterm)
   ("C-c C-t" . vterm-copy-mode)
   ("M-y" . vterm-yank-pop)
   ("C-k" . vterm-send-C-k)
   ("C-c <left>" . multi-vterm-prev)
   ("C-c n" . multi-vterm-prev)
   ("C-c <right>" . multi-vterm-next)
   ))

(defun vterm-send-C-k ()
  "Send `C-k' to libvterm."
  (interactive)
  (kill-ring-save (point) (vterm-end-of-line))
  (vterm-send-key "k" nil nil t))

(require 'consult)
(defun hn/switch-to-vterm-buffer ()
  "Switch to a buffer whose name matches '*vterm'."
  (interactive)
  (if (and (boundp 'multi-vterm-buffer-list) multi-vterm-buffer-list)
      (let* ((buffer-names (remove nil (mapcar 'buffer-name multi-vterm-buffer-list)))
             (chosen-buffer (consult--read buffer-names
                       :prompt "Switch to vterm buffer: "
                       :sort t
                       :require-match t
                       :state (consult--buffer-preview)
                       :history 'buffer-name-history)))
        (when (not (string= chosen-buffer ""))
          (switch-to-buffer chosen-buffer)))
    (new-terminal)))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c l" . mc/edit-lines)
   ("C-c C-SPC" . mc/mark-all-dwim)
   ("C-<" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-M-<" . mc/mark-previous-like-this-symbol)
   ("C-M->" . mc/mark-next-like-this-symbol)
   )
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

;; (defun generate-shortcuts (strings)
;;   "Generate a list of unique shortcuts from STRINGS."
;;   (let ((shortcuts (make-hash-table :test 'equal)))
;;     (dolist (str strings)
;;       (let ((found nil)
;;             (i 0))
;;         (while (and (< i (length str)) (not found))
;;           (let ((char (substring str i (1+ i))))
;;             (if (not (gethash char shortcuts))
;;                 (progn
;;                   (puthash char str shortcuts)
;;                   (setq found t))
;;               (when (and (= i 0) (not (gethash (upcase char) shortcuts)))
;;                 (puthash (upcase char) str shortcuts)
;;                 (setq found t))))
;;           (setq i (1+ i)))
;;         (unless found
;;           (let ((j 0))
;;             (while (and (< j (length str)) (not found))
;;               (let ((char (substring str j (1+ j))))
;;                 (if (not (gethash char shortcuts))
;;                     (progn
;;                       (puthash char str shortcuts)
;;                       (setq found t))
;;                   (when (not (gethash (upcase char) shortcuts))
;;                     (puthash (upcase char) str shortcuts)
;;                     (setq found t))))
;;               (setq j (1+ j))))
;;           (unless found
;;             (error "Cannot find unique shortcut for string: %s" str)))))
;;     (let ((result '()))
;;       (maphash (lambda (k v) (push (cons k v) result)) shortcuts)
;;       result)))

;; (defun create-transient-from-strings (name strings)
;;   "Create a transient with NAME from a list of STRINGS."
;;   (let ((shortcuts (generate-shortcuts strings))
;;         (transient-body '()))
;;     (dolist (pair shortcuts)
;;       (let ((shortcut (car pair))
;;             (string (cdr pair)))
;;         (push (list shortcut string (intern (concat "my-command-" string))) transient-body)
;;         (eval `(defun ,(intern (concat "my-command-" string)) ()
;;                  ,(format "Command for %s" string)
;;                  (interactive)
;;                  (message "%s executed" ,string)))))
;;     (eval
;;      `(transient-define-prefix ,(intern (concat "my-transient-" name)) ()
;;         ,(format "Transient for %s" name)
;;         ["Commands"
;;          ,@transient-body]))))

;; ;; Example usage
;; (defun open-my-transient-makefile ()
;;   (interactive)
;;   (let ((my-make-commands (run-command-recipes-make--commands (run-command-recipes-make--project-makefile))))
;;     (create-transient-from-strings "makefile" my-make-commands)
;;     (my-transient-makefile))
;;   )

;; Yasnippet
(use-package yasnippet
  :ensure t
  :hook ((lsp-mode . yas-minor-mode))
  :config
  )

;; Nyan mode
;; (use-package nyan-mode
;;   :if window-system
;;   :ensure t
;;   :config
;;   (nyan-mode)
;;   ;; (nyan-start-animation)
;;   )

(use-package edit-server
  :if window-system
  :ensure t
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

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
