(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   '("4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e"
     "b0334e8e314ea69f745eabbb5c1817a173f5e9715493d63b592a8dc9c19a4de6"
     "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d"
     "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58"
     "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a"
     "eb122e1df607ee9364c2dfb118ae4715a49f1a9e070b9d2eb033f1cefd50a908"
     "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588"
     "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298"
     "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6"
     "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1"
     "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9"
     "1c596673c1d111e95a404bd12f8dd446cbcd47eee885271e21ffc98c3ac386cb"
     "3e038e9133010baa92e17a2c57f87336e91e6e76139d8c38d7d55d3c59a15967"
     "682a1161ee456e2d715ba30be61697fdbce8c08e23c2c6a1943f155e3e52f701"
     "147a0b0fce798587628774ae804a18a73f121e7e5c5fdf3a874ba584fdbe131d"
     "4e96c6ca1ab443d9804bcb55104848b25bdfda5ae665adeb218db1af07e7979a"
     "e503f6b2f45ecc5c5e295d1b3d84bb484206c4badbf716847a2445facf9f7495"
     "fe2a620695413fe5dcd74e03f0383e577effd7bb59527aa4d86444108d861504"
     "2f57ee6507f30d3228cdddadd0150e7b2fd85dd7c818c2d6485888c7249c37e8"
     default))
 '(erlang-check-module-name nil)
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#151515")
 '(flycheck-check-syntax-automatically '(save))
 '(flycheck-checkers
   '(eglot-check lux yang ada-gnat asciidoctor asciidoc awk-gawk
                 bazel-build-buildifier bazel-module-buildifier
                 bazel-starlark-buildifier bazel-workspace-buildifier
                 c/c++-clang c/c++-gcc c/c++-cppcheck cfengine
                 chef-foodcritic coffee coffee-coffeelint coq
                 css-csslint css-stylelint cuda-nvcc cwl d-dmd
                 dockerfile-hadolint elixir-credo emacs-lisp
                 emacs-lisp-checkdoc ember-template eruby-erubis
                 eruby-ruumba fortran-gfortran go-gofmt go-golint
                 go-vet go-build go-test go-errcheck go-unconvert
                 go-staticcheck groovy haml handlebars
                 haskell-stack-ghc haskell-ghc haskell-hlint html-tidy
                 javascript-eslint javascript-jshint
                 javascript-standard json-jsonlint json-python-json
                 json-jq jsonnet less less-stylelint llvm-llc
                 lua-luacheck lua markdown-markdownlint-cli
                 markdown-mdl nix nix-linter opam perl perl-perlcritic
                 php php-phpmd php-phpcs processing proselint
                 protobuf-protoc protobuf-prototool pug puppet-parser
                 puppet-lint python-flake8 python-pylint
                 python-pycompile python-pyright python-mypy r-lintr
                 racket rpm-rpmlint rst-sphinx rst ruby-rubocop
                 ruby-standard ruby-reek ruby-rubylint ruby ruby-jruby
                 scala scala-scalastyle scheme-chicken scss-lint
                 scss-stylelint sass/scss-sass-lint sass scss sh-bash
                 sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim
                 slim-lint sql-sqlint systemd-analyze tcl-nagelfar
                 terraform terraform-tflint tex-chktex tex-lacheck
                 texinfo textlint typescript-tslint verilog-verilator
                 vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml
                 yaml-ruby yaml-yamllint))
 '(flycheck-color-mode-line-show-running t)
 '(flymake-fringe-indicator-position 'left-fringe)
 '(flymake-note-bitmap '(exclamation-mark compilation-info))
 '(highlight-indent-guides-delay 0)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive 'stack)
 '(hl-line-overlay-priority 500)
 '(kind-icon-default-style
   '(:padding 0 :stroke 0 :margin -1 :radius 0 :height 0.5 :scale 1.0
              :background nil))
 '(lsp-erlang-server 'erlang-ls)
 '(lsp-file-watch-threshold 1000)
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style 'chamfer)
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(0blayout 0x0 0xc 2048-game 2bit :request ac-helm ace-jump-buffer
              ace-jump-helm-line ace-jump-mode ag all-the-icons
              all-the-icons-completion ansible apib-mode
              autotetris-mode benchmark-init blacken cape cargo
              cargo-transient ccls chatgpt cider
              color-theme-sanityinc-tomorrow command-log-mode
              company-box company-erlang company-jedi
              company-prescient consult consult-eglot
              consult-eglot-embark consult-flycheck copilot-chat corfu
              corfu-doc corfu-terminal csv ctrlf dap-erlang dap-mode
              dash dash-functional deadgrep default-text-scale deft
              diff-hl diminish dired-icon dired-preview dired-sidebar
              dired-subtree direnv docker docker-compose-mode
              dockerfile-mode doom-modeline dumb-jump dumb-jump-mode
              eat edit-server editorconfig edts eglot eglot-booster
              eldoc-box elm-mode elpy emacs-everywhere embark
              embark-consult emojify ensime eproject erlang esup
              eterm-256color exec-path-from-shell expand-region
              flatland-theme flycheck flycheck-color-mode-line
              flycheck-eglot flycheck-golangci-lint flycheck-pos-tip
              flycheck-rust flycheck-yang flymake-shellcheck flymd
              fold-dwim fold-this forecast git-gutter go-mode
              gore-mode gptel gruvbox-theme haskell-mode helm helm-ag
              helm-fuzzy-find helm-git helm-lobsters helm-ls-git
              helm-lsp helm-pass helm-projectile helm-rg helm-swoop
              helm-themes helm-xref highlight-indent-guides
              hippie-expand ht iedit indent-bars ivy jenkinsfile-mode
              jinx k8s-mode kind-icon know-your-http-well kubectx-mode
              kubed kubernetes link-hint list-packages-ext lsp-mode
              lsp-tailwindcss lsp-ui lua-mode lux-mode magit
              marginalia markdown-preview-mode markdown-ts-mode
              mini-frame minions multi-term multi-vterm mwim orderless
              org org-drawio org-journal org-modern org-static-blog
              org-tree-slide origami outline-magic outline-toc
              pdf-tools pg popper popwin powerline prescient
              project-treemacs projectile-ripgrep protobuf-mode
              protobuf-ts-mode py-autopep8 rainbow-delimiters request
              restclient restclient-jq restclient-mode rg ripgrep
              run-command run-command-recipes rust-mode selectrum
              shell-maker slime smart-mode-line soothe-theme
              soundcloud spacemacs-theme string-inflection sudo-edit
              swagg swiper swiper-helm tablist templ-ts-mode toml-mode
              typescript-mode undo-tree use-package verb vertico vterm
              vterm-toggle web-mode wgrep wgrep-ag which-key
              whitespace winner yaml-mode yang-mode yasnippet zig-mode))
 '(package-vc-selected-packages
   '((pgmacs :vc-backend Git :url "https://github.com/emarsden/pgmacs")
     (ultra-scroll :vc-backend Git :url
                   "https://github.com/jdtsmith/ultra-scroll")
     (eglot-booster :vc-backend Git :url
                    "https://github.com/jdtsmith/eglot-booster")))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#282828"))
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
 '(powerline-default-separator 'arrow)
 '(safe-local-variable-values '((vim . sw=2) (allout-layout . t)))
 '(set-fringe-style (nil . 0))
 '(sml/shorten-directory t)
 '(sml/show-file-name t)
 '(swiper-goto-start-of-match t)
 '(typescript-indent-level 2)
 '(vc-follow-symlinks t)
 '(vterm-term-environment-variable "xterm-256color")
 '(warning-suppress-types '((comp)))
 '(whitespace-style '(face trailing tabs lines-tail empty)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "ADBO" :family "Source Code Pro"))))
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
