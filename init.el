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

(setq magit-ediff-dwim-show-on-hunks t) ;; fix ediff'ing on staged/unstaged selections

(menu-bar-mode 0)
(tool-bar-mode 0)

(global-hl-line-mode 1)
(column-number-mode 1)

(put 'narrow-to-region 'disabled nil)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (web-mode magit ace-jump-mode zig-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
