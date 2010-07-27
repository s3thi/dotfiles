;; Ankur Sethi's .emacs

(server-start)

(tool-bar-mode nil)
(require 'paren)
(show-paren-mode t)
(column-number-mode t)
(setq mac-option-modifier 'meta)

;; Spaces, not tabs.
(setq-default indent-tabs-mode nil)

;; Show region.
(transient-mark-mode t)

(setq search-highlight t)
(setq scroll-step 2)
(setq kill-whole-line t)

;; Toggle full-screen.
(defun toggle-fullscreen ()
  (interactive) 
  (set-frame-parameter
   nil
   'fullscreen
   (if (frame-parameter nil 'fullscreen)
       nil
     'fullboth)))

(global-set-key [(meta return)] 'toggle-fullscreen)

(fset 'yes-or-no-p 'y-or-n-p)
(require 'color-theme)
(color-theme-initialize)
(scroll-bar-mode nil)
(setq make-backup-files nil)

;; Go to previous window.
(defun other-window-backward (&optional n)
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key [(C .)] 'other-window)
(global-set-key [(C ,)] 'other-window-backward)

;; Enable Org Mode.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Need autofill in text and org modes.
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; flyspell mode
(add-hook 'text-mode-hook '(lambda () (flyspell-mode 1)))
(add-hook 'org-mode-hook '(lambda () (flyspell-mode 1)))

(add-to-list 'default-frame-alist (cons 'width 100))
(add-to-list 'default-frame-alist (cons 'height 45))
(add-to-list 'default-frame-alist (cons 'left 25))
(add-to-list 'default-frame-alist (cons 'top 25))