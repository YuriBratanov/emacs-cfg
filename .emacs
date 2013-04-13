;;; Lisp (SLIME) interaction
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")  ; your SLIME directory
(require 'slime)
(slime-setup)

;;; Load External ELs
(add-to-list 'load-path "~/.emacs.d/external")

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; Swap save and search functions
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-x C-s") 'isearch-forward)

;;; Replace selection
(delete-selection-mode 1)

;;; No Word Wrap
(setq-default truncate-lines t)
(toggle-truncate-lines 1)
(setq truncate-partial-width-windows nil)

;;; Executing external programs
(require 'execute)

(global-font-lock-mode t)
(show-paren-mode 1)
(add-hook 'lisp-mode-hook '(lambda ()
(local-set-key (kbd "RET") 'newline-and-indent)))

;;; Show Line Numbers
(global-linum-mode t)

(require 'exec-path-from-shell) ;; if not using the ELPA package
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "JRUBY_OPTS")

;;; Load Color Theme
(add-to-list 'load-path "/usr/share/emacs24/site-lisp/emacs-goodies-el/")
(require 'color-theme)
(require 'sd-theme)

;;; Fullscreen on/off F11
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
(global-set-key [f11] 'toggle-fullscreen)

;;; Maximize on/off F12
(defun toggle-maximize (&optional f)
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
    '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
    '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(global-set-key [f12] 'toggle-maximize)

;;; Save Workspace
(load "revive")
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)
;;Keyboard shortcuts
(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)

;;; Gherkin
(setq feature-default-language "en")
(setq feature-default-i18n-file "~/.emacs.d/external/gherkin/gem/i18n.yml")
(add-to-list 'load-path "~/.emacs.d/external/gherkin/")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;;; Ruby
(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

;;; Kill Buffer without Confirmation
(defun volatile-kill-buffer ()
   "Kill current buffer unconditionally."
   (interactive)
   (let ((buffer-modified-p nil))
     (kill-buffer (current-buffer))))

(global-set-key (kbd "C-x k") 'volatile-kill-buffer)

;; Shift the selected region right if distance is postive, left if
;; negative
(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)

;;; Auto Complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(ac-config-default)

;;; various
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq inhibit-startup-message t)
(setq inhibit-start-screen t)
(setq inhibit-splash-screen t)

(setq make-backup-files nil)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(global-auto-revert-mode 1)

;;; Buffer Move
(require 'buffer-move)
(global-set-key (kbd "C-c <up>")     'buf-move-up)
(global-set-key (kbd "C-c <down>")   'buf-move-down)
(global-set-key (kbd "C-c <left>")   'buf-move-left)
(global-set-key (kbd "C-c <right>")  'buf-move-right)

;;; Add Scala Support
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))

(add-to-list 'load-path "/home/yuri/emacs-cfg/emacs.d/elpa/scala-mode2-20130403.1734/")
(require 'scala-mode2)

(add-hook 'scala-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "C-M-j") 'join-line)
  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)
))

(add-hook 'scala-mode-hook '(lambda ()
  (require 'whitespace)

  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  (whitespace-mode)
))
