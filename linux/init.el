;; Emacs conf file - Nabil - Linux version

;; adding MELPA support
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents)
  (package-initialize)
  )

;; Intalling use-package - requieres Melpa
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Spacemacs
(setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
(load-file (concat spacemacs-start-directory "init.el"))

;; Doom themes and modeline
(add-to-list 'load-path "/home/leonabil/.emacs.d/packages/doom-modeline-master")
(add-to-list 'load-path "/home/leonabil/.emacs.d/packages/themes-master")
(add-to-list 'load-path "/home/leonabil/.emacs.d/packages/dash.el-master")
(add-to-list 'load-path "/home/leonabil/.emacs.d/packages/all-the-icons.el-master")
(add-to-list 'load-path "/home/leonabil/.emacs.d/packages/shrink-path.el-master")
(add-to-list 'load-path "/home/leonabil/.emacs.d/packages/s.el-master")
(add-to-list 'load-path "/home/leonabil/.emacs.d/packages/f.el-master")
(require 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-one t)

;; commands to avoid wrapping text in horizontally-split windows
;;(set-default 'truncate-lines )
(setq truncate-partial-width-windows nil)

;; Graphical user interface settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Escape running as escape
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Zooming
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Set visible bell
(setq visible-bell t)

;; server mode
(server-start)

;; Quick yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Saving workspace
(desktop-save-mode 1)

;; prevent silly initial splash screen
(setq inhibit-splash-screen t)

;; Delete selected region when typing
(delete-selection-mode 1)

;; ignore case when searching
(setq cases-fold-search t)

;; Windows buffer configuration to undo confs.
(winner-mode 1)

;; Recent files support
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(run-at-time nil (* 10 60) 'recentf-save-list)

;; Window move
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; Interactive do
(require 'ido)
(setq indo-enable-flex-matching t)
;;(setq ido-everywhere t)
(ido-mode 1)

;; path to eshell
(add-to-list 'exec-path "/usr/local/bin/")

;; auto save files location
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; enabling better way of selecting active buffers
(require 'bs)
(global-set-key "\C-x\C-b" 'bs-show)

;; autocomplete
(add-to-list 'load-path "/home/leonabil/.emacs.d/packages/auto-complete")
(add-to-list 'load-path "/home/leonabil/.emacs.d/packages/popup-el-master")
(require 'auto-complete)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; which-key
(add-to-list 'load-path "/home/leonabil/.emacs.d/packages/which-key")
(require 'which-key)
(which-key-mode)

;; beacon
(add-to-list 'load-path "/home/leonabil/.emacs.d/packages/beacon")
(require 'beacon)
(beacon-mode)

;; Ace-window
(add-to-list 'load-path "/home/leonabil/.emacs.d/packages/ace-window-master")
(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;;clang-format support
(global-set-key [C-M-tab] 'clang-format-region)

;; copy one line function
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg))

;; Duplicafe Line
(global-set-key (kbd "C-d") 'duplicate-line)

;;============================================================
;; Smart tabs
;;============================================================
;; On first invocation, this function moves the point to the first non-space character, and on second
;; invocation to the beginning of line. Subsequent invocations will switch between the two positions.

(defun beginning-of-code-or-line ()
  (interactive)

  (if (member 'shift (event-modifiers (aref (this-single-command-keys) 0)))
      (or mark-active (set-mark-command nil))
    (setq mark-active nil))

  (if (and (eq this-command last-command)
           (not (eq (point) (save-excursion (beginning-of-line) (point)))))
      (beginning-of-line)ile
      (back-to-indentation)))

;; This function adds the same same type of functionality at the end of the line. If line is just a comment,
;; moves to end of line. Otherwise, on first invocation, it moves the point to the end of code, and on second
;; invocation to the end of line. Subsequent invocations will switch between the two positions.

(defun end-of-code-or-line ()
  (interactive)

  (if (member 'shift (event-modifiers (aref (this-single-command-keys) 0)))
      (or mark-active (set-mark-command nil))
    (setq mark-active nil))

  (if (or (not comment-start)
          (and (eq this-command last-command)
               (not (eq (point) (save-excursion (end-of-line) (point))))))
      (end-of-line)
    (progn ;; (back-to-end-of-code)
      (let ((eol  (progn (end-of-line)       (point)))
            (stop (progn (beginning-of-line) (point))))
        (if (looking-at (concat "\\s-*" (regexp-quote comment-start) ".*\n"))
            (end-of-line)
          (while (if (re-search-forward "\"[^\"]*\"" eol t) (setq stop (point))))

          (end-of-line)
          (while (re-search-backward (regexp-quote comment-start) stop t))
          (re-search-backward "\\S-" stop t)
          (if (not (looking-at "\n")) (forward-char 1)))))))

;; This smart tab is minibuffer compliant: it acts as usual in the
;; minibuffer. Else if in a sub-shell, dynamic completion is performed
;; by comint-dynamic-complete. Else if mark is active, indents
;; region. Else if point is at the end of a symbol, expands it with
;; dabbrev-expand. Else indents the current line according to the
;; major-mode.

(defun smart-tab ()
  (interactive)

  (if (and (>= emacs-major-version 22)
           (minibufferp))   ;; Unknown to Emacs 21
      (minibuffer-complete)
    (if (memq major-mode '(shell-mode))
        (comint-dynamic-complete)
      (if mark-active
          (indent-region (region-beginning)(region-end))
        (if (looking-at "\\_>")                               ;; Note the use of "\\_>" instead of "\\>"
            (dabbrev-expand nil)
          (indent-for-tab-command))))))

;; Very useful to deal with indents, tabs and spaces, at the beginning or end of lines
(if (>= emacs-major-version 22)
    (global-set-key   [tab]            'smart-tab))                  ;; Do not map smart-tab with Emacs 21 because no minibuffer completion is a real pain

;; Tabs stuff. This is good to avoid tabs at the begining of the line (inserted by smart-tabs).
(setq-default indent-tabs-mode nil)
(add-hook 'text-mode-common-hook
          (lambda () (setq indent-tabs-mode t)))
(setq-default tab-width 2)

;; Disabling right option key to access #
;;(when (eq system-type 'darwin)
;;  (setq mac-right-option-modifier 'none))

;;============================================================
;; c++ stuff
;;============================================================

;; Keep the linux indentation format for c mode; i.e., brackets below the first letter of the funtion.
(setq c-default-style "linux"
      c-basic-offset 2)

;; c++ header files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; c++ moving function up
(defun my-move-function-up ()
  "Move current function up."
  (interactive)
  (save-excursion
    (c-mark-function)
    (let ((fun-beg (point))
          (fun-end (mark)))
      (transpose-regions (progn
                           (c-beginning-of-defun 1)
                           (point))
                         (progn
                           (c-end-of-defun 1)
                           (point))
                         fun-beg fun-end))))

;; c++ moving function down
(defun my-move-function-down ()
  "Move current function down."
  (interactive)
  (save-excursion
    (c-mark-function)
    (let ((fun-beg (point))
          (fun-end (mark)))
      (transpose-regions fun-beg fun-end
                         (progn
                           (c-beginning-of-defun -1)
                           (point))
                         (progn
                           (c-end-of-defun 1)
                           (point))))))

;; C++ key bindings jumping between header and content files
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c h") 'ff-find-other-file)
            (local-set-key  (kbd "C-c <up>") 'my-move-function-up)
            (local-set-key  (kbd "C-c <down>") 'my-move-function-down)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#5a5475" "#CC6666" "#C2FFDF" "#FFEA00" "#55b3cc" "#FFB8D1" "#96CBFE" "#F8F8F0"])
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes '(doom-dracula))
 '(custom-safe-themes
   '("8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "5f19cb23200e0ac301d42b880641128833067d341d22344806cdad48e6ec62f6" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "850bb46cc41d8a28669f78b98db04a46053eca663db71a001b40288a9b36796c" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "4699e3a86b1863bbc695236036158d175a81f0f3ea504e2b7c71f8f7025e19e3" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "a82ab9f1308b4e10684815b08c9cac6b07d5ccb12491f44a942d845b406b0296" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "1f1b545575c81b967879a5dddc878783e6ebcca764e4916a270f9474215289e5" "3d47380bf5aa650e7b8e049e7ae54cdada54d0637e7bac39e4cc6afb44e8463b" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "6c98bc9f39e8f8fd6da5b9c74a624cbb3782b4be8abae8fd84cbc43053d7c175" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" default))
 '(exwm-floating-border-color "#282633")
 '(fci-rule-color "#B8A2CE")
 '(highlight-tail-colors ((("#64657f") . 0) (("#605f82") . 20)))
 '(ispell-dictionary nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#464258" "#C5A3FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#464258" "#C2FFDF"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#464258" "#656565"))
 '(menu-bar-mode nil)
 '(objed-cursor-color "#CC6666")
 '(pdf-view-midnight-colors (cons "#F8F8F0" "#5a5475"))
 '(rustic-ansi-faces
   ["#5a5475" "#CC6666" "#C2FFDF" "#FFEA00" "#55b3cc" "#FFB8D1" "#96CBFE" "#F8F8F0"])
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#5a5475")
 '(vc-annotate-color-map
   (list
    (cons 20 "#C2FFDF")
    (cons 40 "#d6f894")
    (cons 60 "#eaf14a")
    (cons 80 "#FFEA00")
    (cons 100 "#f6dc00")
    (cons 120 "#eece00")
    (cons 140 "#E6C000")
    (cons 160 "#eebd45")
    (cons 180 "#f6ba8b")
    (cons 200 "#FFB8D1")
    (cons 220 "#ee9cad")
    (cons 240 "#dd8189")
    (cons 260 "#CC6666")
    (cons 280 "#b26565")
    (cons 300 "#986565")
    (cons 320 "#7e6565")
    (cons 340 "#B8A2CE")
    (cons 360 "#B8A2CE")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "AnonymicePro Nerd Font Mono" :foundry "nil" :slant normal :weight medium :height 70 :width normal)))))
