;; (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
;;       gc-cons-percentage 0.6)
;; (defvar config:file-name-handler-alist-cache file-name-handler-alist)
;; (setq file-name-handler-alist nil)
;; (defun config:restore-post-init-settings ()
;;   (setq gc-cons-threshold 16777216 ; 16mb
;;         gc-cons-percentage 0.1)
;;   (setq file-name-handler-alist config:file-name-handler-alist-cache))
;; (add-hook 'emacs-startup-hook #'config:restore-post-init-settings)
(setq comp-deferred-compilation t)
(setq inhibit-startup-message t)

(setq gc-cons-threshold (* 100 1000 1000))
;; (setq gc-cons-threshold 1677721600 ; 1600mb
  ;;       gc-cons-percentage 0.6)
;; Turn of menubar, toolbar and scrollbar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
;; Color theme
(load-theme 'wombat)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 200 :width normal :foundry "ADBO" :family "Source Code Pro")))))
