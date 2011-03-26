;; Ankur Sethi's .emacs

(defvar *the-numbers* '(4 8 15 16 23 42))

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(tool-bar-mode 0)
(require 'paren)
(show-paren-mode t)
(column-number-mode t)
(setq mac-option-modifier 'meta)
(menu-bar-mode nil)

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
;; (require 'color-theme)
;; (color-theme-initialize)
(scroll-bar-mode nil)
(setq make-backup-files nil)

;; Go to previous window.
(defun other-window-backward (&optional n)
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key (kbd "C-0") 'other-window)
(global-set-key (kbd "C-9") 'other-window-backward)
(global-set-key (kbd "C-<return>") 'dabbrev-expand)
(global-set-key (kbd "C-1") 'delete-other-windows)

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

;; Screen size on startup. This is mostly hit and miss.
;; Worse, you can't really change these in realtime.
;; You're better off just using the mouse to resize the Emacs
;; window.
;; (add-to-list 'default-frame-alist (cons 'width 115))
;; (add-to-list 'default-frame-alist (cons 'height 37))
;; (add-to-list 'default-frame-alist (cons 'left 0))
;; (add-to-list 'default-frame-alist (cons 'top 0))

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/opt/local/bin/ccl64")

;; Colors in shell.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(require 'command-frequency)
(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)

;; Modeline is not 3D.
(set-face-attribute 'mode-line nil :box nil)
(setq display-time-24hr-format t)
(display-time)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-2") 'goto-line)

(put 'dired-find-alternate-file 'disabled nil)
