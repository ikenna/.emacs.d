(let* ((my-lisp-dir "~/.emacs.d/")
        (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))

(setq frame-title-format "%b")
(column-number-mode t)
(setq inhibit-splash-screen t)

(menu-bar-mode -1)

(when (boundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(if (window-system)
    (progn
      (set-frame-height (selected-frame) 43)
      (set-frame-width (selected-frame) 132)))

(fset 'yes-or-no-p 'y-or-n-p)

(transient-mark-mode t)
(delete-selection-mode 1)
(setq x-select-enable-clipboard t)
(savehist-mode t)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-q") 'delete-other-windows)
(global-set-key (kbd "C-?") 'comment-or-uncomment-region)
(global-set-key (kbd "C-<f11>") 'slime)

(global-set-key (kbd "<M-left>") 'previous-buffer)
(global-set-key (kbd "<M-right>") 'next-buffer)
(global-set-key (kbd "<M-up>")  'windmove-up)
(global-set-key (kbd "<M-down>")  'windmove-down)

(cua-mode)
(ido-mode)

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'zenburn)
(zenburn)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(ac-set-trigger-key "TAB")

(require 'hl-sexp)
(global-hl-sexp-mode)
(set-face-attribute 'hl-sexp-face nil :background "#202020")

(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
(add-hook 'slime-repl-mode-hook 'highlight-parentheses-mode)
(add-hook 'emacs-lisp-mode-hook  'highlight-parentheses-mode)
(setq hl-paren-colors
      '("orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))
