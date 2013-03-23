;;; Lisp (SLIME) interaction
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")  ; your SLIME directory
(require 'slime)
(slime-setup)

;;; Load External ELs
(add-to-list 'load-path "~/.emacs.d/external")

(global-font-lock-mode t)
(show-paren-mode 1)
(add-hook 'lisp-mode-hook '(lambda ()
(local-set-key (kbd "RET") 'newline-and-indent)))

;;; Show Line Numbers
(global-linum-mode t)

;;; Load Color Theme
(add-to-list 'load-path "/usr/share/emacs24/site-lisp/emacs-goodies-el/")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-gtk-ide)))

;;; Fullscreen on/off F11
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
(global-set-key [f11] 'toggle-fullscreen)

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

