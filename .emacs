(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" default)))
 '(package-selected-packages
   (quote
    (xclip projectile-sift projectile company tmux-pane yaml-mode wrap-region monokai-theme helm-ag fzf expand-region ag)))
 '(tab-width 4))

;; ^-- Automatically managed stuff

;; Hooks
(add-hook 'after-init-hook (lambda () (load-theme 'monokai)))

;; Paths
(add-to-list 'load-path "~/.fzf")
(add-to-list 'load-path "~/.emacs.d/lib")

;; Requires
(require 'ansi-color) ;; workaround on monokay error
(require 'uniquify) ;; show folder in buffer name
(require 'xclip)

;; Settings
(setq-default indent-tabs-mode nil)
(setq completion-cycle-threshold t)
(setq show-trailing-whitespace t)
(setq inhibit-splash-screen t)
(setq uniquify-buffer-name-style 'reverse)
(setq tramp-default-method "ssh")

;; Modes
(xclip-mode t)
(show-paren-mode t)
(projectile-mode +1)
(smartparens-mode t)


;; Custom comands

(defun dh-update-dot-files ()
  (interactive)
  (shell-command "git -C ~/code/dhilst commit -am 'update config' && git -C ~/code/dhilst push origin master"))

;; Key binds
(global-set-key (kbd "C-x RET C-c") 'cd)
(global-set-key (kbd "C-x RET C-d") 'dired)
(global-set-key (kbd "C-x RET C-f") 'fzf)
(global-set-key (kbd "C-x C-g") 'helm-grep-do-git-grep)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-@") 'er/expand-region)
(global-set-key (kbd "C-x C-o") 'ace-window)
(windmove-default-keybindings)

;; Initializations
(server-start)


