;; Ankur Sethi's .emacs

(add-to-list 'load-path "~/geiser/")

(defun load-if-exists (filename)
  (let ((expanded-name (expand-file-name filename)))
	(if (file-exists-p filename)
		(load filename))))

(if (load-if-exists "~/quicklisp/slime-helper.el")
	(setq inferior-lisp-program "sbcl"))

(load-if-exists "~/geiser/elisp/geiser.el")
(setq geiser-racket-binary "/usr/local/bin/racket")

(if (boundp 'tool-bar-mode)
    (tool-bar-mode 0))

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

(transient-mark-mode t)
(setq search-highlight t)
(setq scroll-step 2)

(fset 'yes-or-no-p 'y-or-n-p)

(if (boundp 'scroll-bar-mode)
	(scroll-bar-mode nil))

(setq make-backup-files nil)

;; Go to previous window.
(defun other-window-backward (&optional n)
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key (kbd "C-0") 'other-window)
(global-set-key (kbd "C-9") 'other-window-backward)
(global-set-key (kbd "C-<return>") 'dabbrev-expand)
(global-set-key (kbd "C-1") 'delete-other-windows)
;; To make Fn+Delete work again on Lion.
(global-set-key '[(kp-delete)] 'delete-char)
(global-set-key (kbd "<f1>") "λ")

;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

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

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-f") 'fill-region)
