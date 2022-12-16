(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(global-set-key [(C w)] ctl-x-map)
(global-set-key [(C h)] 'ace-jump-mode)
(global-set-key [(C x) (C l)] 'toggle-truncate-lines)
(global-set-key [(C M return)] 'toggle-frame-maximized)
(global-set-key [(C x) (C w)] 'kill-region)
(global-set-key [(C w) (C i)] 'other-window)
(global-set-key [(C o)] (lambda () (interactive) (occur (current-word))))
(global-set-key [(C x) (C k)] 'kill-buffer)
(global-set-key [(C x) (C m)] 'magit-status)
(global-set-key [(C x) (C b)] 'mode-line-other-buffer)
(global-set-key [(M \.)] 'hi-lock-current-word)
(global-set-key [(M \,)] 'hi-unlock-current-word)
(global-set-key [(C .)] 'helm-etags-select)
(global-set-key [(C \,)] 'xref-pop-marker-stack)


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
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(icomplete-vertical orderless helm d-mode elixir-mode rg scala-mode web-mode magit ace-jump-mode zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

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

(global-set-key [(C x) (f)] 'find-project-file) ;Sorry, set-fill-column :^(

(defun ripgrep-project (query)
  (interactive "sSearch Project (regexp): ")
  (let ((path (locate-dominating-file default-directory ".git")))
    (rg query "*" (or path default-directory))))

(global-set-key [(C x) (C r)] 'ripgrep-project)
