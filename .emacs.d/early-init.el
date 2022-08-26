(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
     gc-cons-percentage 0.6)
(setq comp-deferred-compilation t)
(setq inhibit-startup-message t)

;; Turn of menubar, toolbar and scrollbar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
;; Color theme
(load-theme 'wombat)
