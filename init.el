(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;; (global-set-key [(C w)] ctl-x-map)
(global-set-key [(C h)] 'ace-jump-mode)
(global-set-key [(C x) (l)] 'toggle-truncate-lines)
(global-set-key [(C M return)] 'toggle-frame-maximized)
(global-set-key [(C x) (C f)] 'find-file-at-point)
(global-set-key [(C c) (C w)] 'highlight-tabs)

(defun highlight-tabs ()
  (interactive)
  (let ((whitespace-style '(face tabs)))
    (whitespace-mode 'toggle)))

(global-set-key [(C c) (C c)] (lambda ()
                                (interactive)
                                (save-window-excursion
                                  (recompile))
                                (switch-to-buffer "*compilation*")
                                (end-of-buffer)))
(global-set-key (kbd "M-RET") 'bash-the-current-line)
(global-set-key [f12] (lambda ()
                        (interactive)
                          (if (eq (current-buffer) (get-buffer "*scratch*"))
                              (switch-to-prev-buffer)
                            (switch-to-buffer "*scratch*"))))

;; (global-set-key [(C x) (C w)] 'kill-region)
;; (global-set-key [(C w) (C i)] 'other-window)
;; (global-set-key [(C o)] (lambda () (interactive) (occur (current-word))))
;; (global-set-key [(C x) (C k)] 'kill-buffer)
;; (global-set-key [(C x) (C m)] 'magit-status)
;; (global-set-key [(C x) (C b)] 'mode-line-other-buffer)
;; (global-set-key [(M \.)] 'hi-lock-current-word)
;; (global-set-key [(M \,)] 'hi-unlock-current-word)
;; (global-set-key [(C .)] 'helm-etags-select)
;; (global-set-key [(C \,)] 'xref-pop-marker-stack)


(setq magit-ediff-dwim-show-on-hunks t) ;; fix ediff'ing on staged/unstaged selections

(menu-bar-mode 0)
(tool-bar-mode 0)

;(global-hl-line-mode 1)
(column-number-mode 1)

(put 'narrow-to-region 'disabled nil)

;; Custom functions

(defun bash-the-current-line ()
  "Executes the current line using `shell-command-to-string',
inserts the results directly below in the current buffer."
  (interactive)
  (let ((start-point (point))
        (result (shell-command-to-string (buffer-substring (point-at-bol) (point-at-eol)))))
    (goto-char start-point)
    (end-of-line)
    (newline)
    (insert "===\n")
    (insert result)
    (goto-char start-point)))

(defun hi-current-word ()
  (format "\\b%s\\b" (regexp-quote (current-word))))

(defun hi-lock-current-word ()
  (interactive)
  (hi-lock-mode 1)
  (let* ((hi-lock-auto-select-face t)
         (face (hi-lock-read-face-name)))
    (or (facep face) (setq face 'hi-yellow))
    (highlight-phrase (hi-current-word) face)))
;    (hi-lock-set-pattern (hi-current-word) face)))

(defun hi-unlock-current-word ()
  (interactive)
  (unhighlight-regexp (hi-current-word)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-merge-split-window-function 'split-window-horizontally)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-ignored-server-capabilities
   '(:completionProvider :signatureHelpProvider :documentHighlightProvider :documentSymbolProvider :workspaceSymbolProvider :codeActionProvider :codeLensProvider :documentFormattingProvider :documentRangeFormattingProvider :documentOnTypeFormattingProvider :colorProvider :foldingRangeProvider :executeCommandProvider))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(major-mode-remap-alist
   '((css-mode . css-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (java-mode . java-ts-mode)
     (js-mode . typescript-ts-mode)
     (js2-mode . typescript-ts-mode)
     (json-mode . json-ts-mode)
     (js-json-mode . json-ts-mode)))
 '(make-backup-files nil)
 '(package-selected-packages
   '(ini-mode magit lsp-mode yaml-mode typescript-mode gtags-mode haskell-mode sly icomplete-vertical orderless rg web-mode ace-jump-mode))
 '(read-buffer-completion-ignore-case t)
 '(tab-width 4)
 '(truncate-lines t)
 '(typescript-ts-mode-indent-offset 4)
 '(whitespace-line-column nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 130 :width normal :foundry "nil" :family "Atkinson Hyperlegible")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; (require 'orderless)
;; (setq completion-styles '(orderless basic)
;;      completion-category-overrides '((file (styles basic partial-completion))))

;; (require 'icomplete-vertical)
;; (icomplete-mode t)
;; (icomplete-vertical-mode t)

(defun find-project-file ()
  "Uses ripgrep --files option to select a file for opening"
  (interactive)
  (let* ((path (locate-dominating-file default-directory ".git"))
         (rg-cmd (if path (format "rg --files %s" path)
                   "rg --files"))
         (all-files (with-temp-buffer
                      (shell-command rg-cmd (current-buffer) "*RipGrep Temp Errors*")
                      (string-lines (buffer-string) t)))
         (selected-file (completing-read "File: " all-files nil t)))
    (when selected-file
      (find-file selected-file))))

;; (global-set-key [(C x) (f)] 'find-project-file) ;Sorry, set-fill-column :^(

(defun ripgrep-project (query)
  (interactive "sSearch Project (regexp): ")
  (let ((path (locate-dominating-file default-directory ".git")))
    (rg query "*" (or path default-directory))))

;; (global-set-key [(C x) (C r)] 'ripgrep-project)

(defun find-current-file-in-idea ()
  (interactive)
  (shell-command (format "idea -l %s %s"
                         (1+ (count-lines 1 (point)))
                         (buffer-file-name))))

;; (defun goback ()
;;   (interactive)
;;   (find-current-file-in-idea)
;;   (save-buffers-kill-terminal))

(setq-default fill-column 80)

(setq treesit-language-source-alist
      '((java "https://github.com/tree-sitter/tree-sitter-java")
        (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
        (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (make "https://github.com/alemuller/tree-sitter-make")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))

;; (add-to-list 'major-mode-remap-alist
;;              '(
;;                                         ;(python-mode . python-ts-mode)
;;                (css-mode . css-ts-mode)
;;                (typescript-mode . typescript-ts-mode)
;;                (js-mode . typescript-ts-mode)
;;                (js2-mode . typescript-ts-mode)
;;                                         ;(c-mode . c-ts-mode)
;;                                         ;(c++-mode . c++-ts-mode)
;;                                         ;(c-or-c++-mode . c-or-c++-ts-mode)
;;                                         ;(bash-mode . bash-ts-mode)
;;                (css-mode . css-ts-mode)
;;                (json-mode . json-ts-mode)
;;                (js-json-mode . json-ts-mode)
;;                                         ;(sh-mode . bash-ts-mode)
;;                                         ;(sh-base-mode . bash-ts-mode))
;;                ))

;(defun jsonrpc--log-event (connection message &optional type))
(setq eglot-events-buffer-size 0);; fix eglot's performance :crossed_fingers:

;(require 'portal "~/.emacs.d/portal.el")
