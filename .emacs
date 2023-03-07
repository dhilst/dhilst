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
   '(lsp-ui rustic ocamlformat merlin ocp-indent solidity-flycheck solidity-mode geiser-racket geiser magit rbenv dot-mode multiple-cursors evil csharp-mode slime-company expand-region emojify unicode-fonts paredit racket-mode jedi flycheck-mypy drag-stuff blacken use-package lsp-mode helm-rg lean-mode yasnippet-lean yasnippet-snippets idle-highlight-mode smartparens dracula-theme company-coq monokai-theme fzf helm proof-general))
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

(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; Set PATH from the shel
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))
;; (set-exec-path-from-shell-PATH)
(setenv "PATH" (concat (getenv "PATH") ":/home/geckos/.local/bin:/home/geckos/.local/opam/bin:/home/geckos/.fzf/bin:/home/geckos/.rbenv/shims"))
(setq-default exec-path (append exec-path '("/home/geckos/.local/opam/bin" "/home/geckos/.rbenv/shims")))

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

;; Set SMerge prefix to `C-c v`
(setq smerge-command-prefix "\C-cv")

;; Creates a new temrinal, name it *terminal-<n>*
(defun new-term (&optional n)
  (interactive)
  (let* ((n (or n 1))
	 (term-name (format "terminal-%d" n))
	 (bname (format "*%s*" term-name)))
    (cond
     ((not (get-buffer "*terminal*"))
      (term "/usr/bin/zsh"))
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
(setq coq-compile-before-require t)
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

;; solidity stuff
(require 'solidity-flycheck)
(setq solidity-flycheck-solc-checker-active t)


;; Rust stuff
;; https://robert.kra.hn/posts/rust-emacs-setup/
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  ;; ... see above ...
  (:map company-mode-map
	("<tab>". tab-indent-or-complete)
	("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(use-package flycheck :ensure)

(setq lsp-rust-analyzer-server-display-inlay-hints t)
;; end of Rust stuff
