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
