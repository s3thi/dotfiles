;; Ankur Sethi's .emacs

(defvar *the-numbers* '(4 8 15 16 23 42))

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(require 'go-mode-load)

(let ((filename (expand-file-name "~/quicklisp/slime-helper.el")))
  (if (file-exists-p filename)
      (progn
        (load filename)
        (setq inferior-lisp-program "clisp"))))

(tool-bar-mode 0)
(menu-bar-mode 0)
(require 'paren)
(show-paren-mode t)
(column-number-mode t)
(setq mac-option-modifier 'meta)

(setq-default tab-width 4
              indent-tab-mode t)

(defun gm-c-mode-hook ()
  (setq c-basic-offset 4
        c-default-style "k&r"))

(add-hook 'c-mode-hook 'gm-c-mode-hook)

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
;; (setq-default fill-column 80)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'org-mode-hook 'turn-on-auto-fill)

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

(defun font-existsp (font-name)
  (member font-name (font-family-list)))

;; M-x mac-font-panel-mode gets you the font panel on OS X. M-x describe-font
;; gets you the name of the font.
(cond
 ((eq system-type 'gnu/linux)
  (set-default-font "-unknown-Anonymous Pro-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"))
 ((eq system-type 'darwin)
  (set-default-font "-apple-anonymous-medium-r-normal--16-140-72-72-m-140-iso10646-1")))

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
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-f") 'fill-region)

(put 'dired-find-alternate-file 'disabled nil)
