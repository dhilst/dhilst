;; Basic .emacs with a good set of defaults, to be used as template for usage
;; with OCaml and OPAM
;;
;; Author: Louis Gesbert <louis.gesbert@ocamlpro.com>
;; Released under CC0

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Generic, recommended configuration options

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-fuzzy nil)
 '(backup-directory-alist '(("." . "~/.local/share/emacs/backups")))
 '(compilation-context-lines 2)
 '(compilation-error-screen-columns nil)
 '(compilation-scroll-output t)
 '(compilation-search-path '(nil "src"))
 '(custom-enabled-themes '(monokai))
 '(custom-safe-themes
   '("a1fbd8fcf680289f292184e205bfaf2252b42c77ff7083b9e3f48450cc3d3a52" "c7000071e9302bee62fbe0072d53063da398887115ac27470d664f9859cdd41d" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" default))
 '(delete-selection-mode t)
 '(electric-indent-mode nil)
 '(global-display-fill-column-indicator-mode nil)
 '(helm-completion-style 'helm)
 '(indent-tabs-mode nil)
 '(line-move-visual t)
 '(next-error-highlight t)
 '(next-error-highlight-no-select t)
 '(next-line-add-newlines nil)
 '(package-selected-packages
   '(rbenv dot-mode multiple-cursors evil csharp-mode slime-company expand-region emojify unicode-fonts paredit racket-mode jedi flycheck-mypy drag-stuff blacken use-package lsp-mode helm-rg lean-mode yasnippet-lean yasnippet-snippets idle-highlight-mode smartparens dracula-theme company-coq monokai-theme fzf helm proof-general))
 '(require-final-newline t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(visible-bell t))

;; Disable splash screen
(setq inhibit-startup-screen t)

;; OCaml configuration
;;  - better error and backtrace matching
(defun set-ocaml-error-regexp ()
  (set
   'compilation-error-regexp-alist
   (list '("[Ff]ile \\(\"\\(.*?\\)\", line \\(-?[0-9]+\\)\\(, characters \\(-?[0-9]+\\)-\\([0-9]+\\)\\)?\\)\\(:\n\\(\\(Warning .*?\\)\\|\\(Error\\)\\):\\)?"
    2 3 (5 . 6) (9 . 11) 1 (8 compilation-message-face)))))

(add-hook 'tuareg-mode-hook 'set-ocaml-error-regexp)
(add-hook 'caml-mode-hook 'set-ocaml-error-regexp)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line


;; Set PATH from the shel
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))
;; (set-exec-path-from-shell-PATH)
(setenv "PATH" (concat (getenv "PATH") ":/home/geckos/.local/bin:/home/geckos/.local/opam/bin:/home/geckos/.fzf/bin:/home/geckos/.rbenv/shims"))
(setq-default exec-path (append exec-path '("/home/geckos/.opam/4.12.1/bin/" "/home/geckos/.local/opam/bin" "/home/geckos/.rbenv/shims")))

(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(require 'package)

;; helm stuff
(helm-mode 1)

;; keybinds
;; https://emacs.stackexchange.com/questions/65080/stop-major-modes-from-overwriting-my-keybinding
(defvar my/keys-keymap (make-keymap)
  "Keymap for my/keys-mode")

(define-minor-mode my/keys-mode
  "Minor mode for my personal keybindings."
  :init-value t
  :global t
  :keymap my/keys-keymap)


;; Needed by idle-highlight-mode
(global-hi-lock-mode 0)
(global-idle-highlight-mode -1) ;; nil = enabled, -1 = disabled
(require 'hi-lock)
(defun jpt-toggle-mark-word-at-point ()
  (interactive)
  (if hi-lock-interactive-patterns
      (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
    (highlight-symbol-at-point)))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-p") 'hippie-expand)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-p") 'fzf-git-files)
(global-set-key (kbd "C-c C-S-p") 'fzf-find-file)
(global-set-key (kbd "C-c C-e") 'mc/edit-lines)
(global-set-key (kbd "M-SPC") 'er/expand-region)
(global-set-key (kbd "C-M-SPC") 'er/contract-region)
(global-set-key (kbd "M-g l") "λ")
(global-set-key (kbd "M-g G") "Γ")
(global-set-key (kbd "M-g L") "Λ")
(global-set-key (kbd "M-g P") "Π")
(global-set-key (kbd "M-g S") "Σ")
(global-set-key (kbd "C-*") 'jpt-toggle-mark-word-at-point)

;; Creates a new temrinal, name it *terminal-<n>*
(defun new-term (&optional n)
  (interactive)
  (let* ((n (or n 1))
	 (term-name (format "terminal-%d" n))
	 (bname (format "*%s*" term-name)))
    (cond
     ((not (get-buffer "*terminal*"))
      (term))
     ((not (get-buffer bname))
      (set-buffer (make-term term-name "zsh"))
      (term-mode)
      (term-char-mode)
      (switch-to-buffer bname))
     (t
      (new-term (1+ n))))))

;; coq-company stuff
;; Load company-coq when opening Coq files
(add-hook 'coq-mode-hook (lambda ()
                           (company-coq-mode)
                           ;; Fix the problem with multiple-cursors where
                           ;; . is not inserted in coq-mode
                           (local-set-key (kbd ".") 'self-insert-command)
                           ))
          
(setq coq-prefer-top-of-conclusion t)
;; Move between windows with S-arrow
(windmove-default-keybindings)

;; Python stuff
(set-language-environment "UTF-8")
(require 'flycheck-mypy)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)

;; disable eletric indent mode in Proof General
(eval-after-load "proof-script"
  '(progn
     (setq electric-indent-mode nil)
     ))
(put 'company-coq-fold 'disabled nil)

;; Setup emoji fonts
(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font
    t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

;; Let me use emacsclient on the terminal
(server-start)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Disable menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Disable C-x C-l keybind
(put 'downcase-region 'disabled nil) 


;; Coq macros

;; look for (letP? _ := (let? _ := foo) in the goal
;; and insert [Error.split_error_with foo_is_valid]
(fset 'split_error (kmacro-lambda-form [?\M-o ?\M-< ?\C-s ?l ?e ?t ?P
   ?? ? ?\' return ?\C-s ?: ?= ?\C-s ?\C-f ?\C- ?\C-s ? return ?\M-w
   134217807 ?E ?r ?r ?o ?r ?. ?s ?p ?l ?i ?t ?_ ?e ?r ?r ?o ?r ?_ ?w
   ?i ?t ?h ? ?\C-y backspace ?_ ?i ?s ?_ ?v ?a ?l ?i ?d ?.] 0 "%d"))
