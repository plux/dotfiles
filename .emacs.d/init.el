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



;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-system-monitor)
;; (require 'eaf-browser)
;; (require 'eaf-image-viewer)
;; (require 'eaf-markdown-previewer)

;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :custom
;;   ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (defalias 'browse-web #'eaf-open-browser)
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding))

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

(add-to-list 'load-path "~/.emacs.d/lisp")

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

(global-set-key (kbd "C-q") 'toggle-my-projectile-vterm)
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

(use-package go-ts-mode
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
(use-package :request
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
                           (analyses . ((fieldalignment . t)
                                        (fillreturns . t)))
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
   ("C-c C-e" . consult-flymake)
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
 '(erlang-check-module-name nil t)
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#151515")
 '(flycheck-check-syntax-automatically '(save))
 '(flycheck-checkers
   '(eglot-check lux yang ada-gnat asciidoctor asciidoc awk-gawk bazel-build-buildifier bazel-module-buildifier bazel-starlark-buildifier bazel-workspace-buildifier c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint elixir-credo emacs-lisp emacs-lisp-checkdoc ember-template eruby-erubis eruby-ruumba fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-staticcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json json-jq jsonnet less less-stylelint llvm-llc lua-luacheck lua markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc protobuf-prototool pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile python-pyright python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-standard ruby-reek ruby-rubylint ruby ruby-jruby scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar terraform terraform-tflint tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby yaml-yamllint))
 '(flycheck-color-mode-line-show-running t)
 '(flymake-fringe-indicator-position 'left-fringe)
 '(flymake-note-bitmap '(exclamation-mark compilation-info))
 '(highlight-indent-guides-delay 0)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive 'stack)
 '(hl-line-overlay-priority 500)
 '(kind-icon-default-style
   '(:padding 0 :stroke 0 :margin -1 :radius 0 :height 0.5 :scale 1.0 :background nil))
 '(lsp-erlang-server 'erlang-ls)
 '(lsp-file-watch-threshold 1000)
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style 'chamfer)
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(flycheck-golangci-lint gore-mode shell-maker :request copilot-chat swagg toml-mode protobuf-ts-mode markdown-ts-mode templ-ts-mode web-mode lsp-tailwindcss mwim tablist docker docker-compose-mode diff-hl run-command-recipes run-command dired-icon dired-preview dired-subtree verb restclient-jq restclient ivy lua-mode org-drawio indent-bars 2bit 2048-game request eterm-256color link-hint popwin eat 0blayout multi-vterm vterm-toggle vterm multi-term tree-sitter-langs dap-erlang project-treemacs gptel chatgpt corfu-terminal eglot-booster eldoc-box consult-eglot flycheck-eglot dumb-jump-mode flymake-shellcheck magit transient consult-flycheck ctrlf cargo cargo-transient rg jinx list-packages-ext default-text-scale hippie-expand all-the-icons swiper 0xc 0x0 doom-modeline marginalia emacs-everywhere esup company-prescient prescient selectrum vertico cape kind-icon all-the-icons-completion org org-modern highlight-indent-guides corfu corfu-doc command-log-mode org-tree-slide git-gutter zig-mode ccls company-erlang outline-magic origami fold-dwim fold-this dired-sidebar wgrep-ag wgrep lux-mode direnv org-static-blog minions smart-mode-line powerline flycheck-color-mode-line popper mini-frame consult embark embark-consult orderless dap-mode rainbow-delimiters rust-mode diminish helm-xref eglot outline-toc company-box helm-swoop flycheck-pos-tip emojify flycheck-yang yang-mode dash soothe-theme spacemacs-theme color-theme-sanityinc-tomorrow flatland-theme gruvbox-theme swiper-helm edts py-autopep8 blacken protobuf-mode company-jedi flycheck erlang slime projectile-ripgrep ripgrep iedit deft undo-tree know-your-http-well deadgrep helm-rg dumb-jump pdf-tools string-inflection use-package lsp-mode ensime csv helm-projectile helm-ls-git helm-fuzzy-find ace-jump-buffer ace-jump-helm-line ac-helm helm-ag helm-git helm-themes helm-lobsters helm-pass apib-mode ht dash-functional org-journal yaml-mode multiple-cursors markdown-preview-mode haskell-mode go-mode forecast flymd flycheck-rust eproject elpy elm-mode editorconfig edit-server dockerfile-mode cider autotetris-mode ansible ag ace-jump-mode winner whitespace helm projectile lsp-ui which-key yasnippet helm-lsp benchmark-init exec-path-from-shell))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster")))
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
 '(vterm-term-environment-variable "xterm-256color")
 '(warning-suppress-types '((comp)))
 '(whitespace-style '(face trailing tabs lines-tail empty)))
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
 '(corfu-default ((t (:background "#191a1b"))))
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
 '(my-kind-icon ((t (:background "#191a1b" :height 0.5))))
 '(swiper-background-match-face-2 ((t (:inherit swiper-match-face-2))))
 '(swiper-line-face ((t (:inherit nil :background "#454 545"))))
 '(swiper-match-face-2 ((t (:inverse-video t))))
 '(treemacs-hl-line-face ((t (:inherit hl-line :background "#494949"))))
 '(whitespace-line ((t (:background "gray9")))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
