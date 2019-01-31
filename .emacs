;; -*- mode: elisp -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(global-hl-line-mode 1)

;; Enable transient mark mode
(transient-mark-mode 1)

(set-frame-font "Input Mono Narrow-10" nil t)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen

(add-hook 'org-mode-hook #'(lambda ()
                             ;; make the lines in the buffer wrap around the edges of the screen.
                             ;; to press C-c q  or fill-paragraph ever again!
                             (visual-line-mode)
                             (org-indent-mode)))
;; this changes the org-mode ellipsis to an arrow but for some reason it
;; shows it in the wrong colour, and underlined?!?!
;; (setq org-ellipsis " ▼")

;; use org-bullets-mode for utf8 symbols as org bullets
;;(require 'org-bullets)
;; make available "org-bullet-face" such that I can control the font size individually
;;(setq org-bullets-face-name (quote org-bullet-face))
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;;(setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))
; (setq org-todo-keywords
;  '((sequence "TODO" "DISCUSS WITH JO" "TESTING" "|" "SPEC FINISHED" "READY TO PUBLISH" "DONE")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(frame-background-mode nil)
 '(package-selected-packages (quote (solarized-theme org-bullets evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-todo ((t (:background "unspecified" :foreground "brightred" :weight bold)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solarized
;; https://github.com/sellout/emacs-color-theme-solarized
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized/")
(customize-set-variable 'frame-background-mode 'dark)
(load-theme 'solarized t)
; (set-frame-parameter nil 'background-mode 'dark)
; (enable-theme 'solarized)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tell emacs to use system default to open PDFs
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.pdf\\'" . "xdg-open \"%s\"")
        ("\\.pdf::\\([0-9]+\\)\\'" . "xdg-open \"%s\" -p %1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                     
;; required for TAB to work in org-mode with evil mode in terminal
;; see: https://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working
(setq evil-want-C-i-jump nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                     
;; load evil mode
(require 'evil)
(evil-mode 1)

