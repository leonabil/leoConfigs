;; Emacs conf Leo Nabil - Mac version

;; Faces
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes '(modus-vivendi))
 '(ns-command-modifier 'meta)
 '(package-selected-packages '(nerd-icons ivy auto-complete which-key org))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "AnonymicePro Nerd Font Mono" :foundry "nil" :slant normal :weight medium :height 160 :width normal)))))

;; adding MELPA support
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; spacemacs
(setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
(load-file (concat spacemacs-start-directory "init.el"))

;; commands to avoid wrapping text in horizontally-split windows
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Graphical user interface
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

;; server mode
(server-start)

;; Quick yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Saving workspace
(desktop-save-mode 1)

;; Delete selected region when typing
(delete-selection-mode 1)

;; preventf silly initial splash screen
(setq inhibit-splash-screen t)
(setq cases-fold-search t)  ; ignore case when searching

;; numbered lines
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Meta modifier in MACOS
(setq mac-option-modifier 'meta)

;; Ace Window and windowd move
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

;; which-key
(require 'which-key)
(which-key-mode)

;; autocomplete
(ac-config-default)
(global-auto-complete-mode t)

;; enabling better way of selecting active buffers
(global-set-key "\C-x\C-b" 'bs-show)

;;ivy mode
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-display-style 'fancy)

;; Org mode
(add-to-list 'load-path "~/src/org-mode/lisp")
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
