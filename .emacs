;;
;; USAGE:
;;    It should be useful enough just as is. Just read the bindings on the code below.
;;    But if you want it to be nice, then run M-x install-all to install every needed package.
;;

;; TODO:
;;   session management: use current project name to save desktop to a separate file
;;   binding for pgup pgdown to a real text scroll
;;   auto label bookmarks
;;   github bindings (maybe using gh)
;;   use git-link

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(bookmark-sort-flag nil)
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(dired-hide-details-hide-symlink-targets nil)
 '(ediff-keep-variants nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(vc-follow-symlinks nil))

;; F10    to see menu
;; C-h b  to see current bindings
;;

;; used to bindings
(cua-mode)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key "\M-<" 'comment-dwim)
(global-set-key (kbd "C-r") 'query-replace-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-e") 'isearch-forward)
(global-set-key (kbd "C-w") 'isearch-forward-word)
(global-set-key (kbd "C-f") 'find-file)
(global-set-key (kbd "M-f") 'find-name-dired)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-x s") 'save-buffer) ;; stop asking

;; bookmarks
(global-set-key (kbd "C-b") 'bookmark-jump)
(global-set-key (kbd "M-b") 'bookmark-set)
(global-set-key (kbd "C-x b") 'bookmark-bmenu-list)

;; highlights
(global-set-key (kbd "<f6>") 'highlight-regexp)
(global-set-key (kbd "C-<f6>") 'unhighlight-regexp)

;; avoid dired garbage
(put 'dired-find-alternate-file 'disabled nil)

;; avoid default manuals screen
(setq inhibit-splash-screen t)

;; auto revert on file changes
(global-auto-revert-mode)

;; smoother scrolling
(setq scroll-conservatively 10000)

;;insert newline char
(define-key isearch-mode-map (kbd "M-RET")
  '(lambda ()(interactive)(isearch-process-search-char ?\n)))

;; cleaner mode-line
(setq-default mode-line-format '("%e"
                                mode-line-modified
                                mode-line-remote
                                " " mode-line-buffer-identification
                                " " mode-line-position
                                (vc-mode vc-mode)
                                " " mode-line-modes
                                mode-line-misc-info
                                mode-line-end-spaces))

;; nice grep
(global-set-key (kbd "C-g") 'rgrep)
(eval-after-load "grep"
  '(progn
    (add-to-list 'grep-find-ignored-files "*.tmp")
    (add-to-list 'grep-find-ignored-files "*.log")
    (add-to-list 'grep-find-ignored-directories ".*")))

;; forced indentation
(defun indent-rigidly-n (n)
  "Indent the region, or otherwise the current line, by N spaces."
  (let* ((use-region (and transient-mark-mode mark-active))
         (rstart (if use-region (region-beginning) (point-at-bol)))
         (rend   (if use-region (region-end)       (point-at-eol)))
         (deactivate-mark "irrelevant")) ; avoid deactivating mark
    (indent-rigidly rstart rend n)))
(defun indent-rigidly-1 ()
  "Indent the region, or otherwise the current line, by 1 spaces."
  (interactive)
  (indent-rigidly-n 1))
(defun outdent-rigidly-1 ()
  "Indent the region, or otherwise the current line, by -1 spaces."
  (interactive)
  (indent-rigidly-n -1))
(global-set-key (kbd "M-i") 'indent-rigidly-1)
(global-set-key (kbd "M-u") 'outdent-rigidly-1)

;; highlight things
(show-paren-mode 1) ;; deactivate if using mic-paren
(global-hl-line-mode)

;; line numbers
(global-set-key (kbd "<f5>") 'linum-mode)
;;(global-linum-mode 1)

;; Disable line numbers in certain buffers
(defcustom linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode dired-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'linum)

(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off. Also turns off numbering in starred modes like scratch"
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
          (string-match "*" (buffer-name)))
    (linum-mode 1)))

;; mouse machinery
(global-set-key (kbd "<f2>") 'xterm-mouse-mode)
(xterm-mouse-mode)

;; hide menu bar, use f10 to use it anyway
(menu-bar-mode -1)

;; C-x 1 to close other windows
;; C-x 2 to split horizontal
;; C-x 3 to split vertical

;; move through windows
(global-set-key (kbd "C-<right>") 'next-multiframe-window)
(global-set-key (kbd "C-<left>") 'previous-multiframe-window)

;; resize windows
(global-set-key (kbd "M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-S-<down>") 'shrink-window)
(global-set-key (kbd "M-S-<up>") 'enlarge-window)

;; buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-k") 'kill-buffer)
(global-set-key (kbd "<f9>") 'dired-other-window)

(defun buffer-mode ()
  "Returns the major mode associated with a buffer."
  (with-current-buffer (buffer-name)
     major-mode))

(defun next-code-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (next-buffer)
    (while
        (and
         (or (string-match-p "^\*" (buffer-name))
             ;;(string-match-p "dired-mode" (format "%s" (buffer-mode)) )
             )
         (not ( equal bread-crumb (buffer-name) )) )
      (next-buffer))))
(global-set-key (kbd "C-<up>") 'next-code-buffer)
;;(global-set-key (kbd "C-<up>") 'next-buffer)

(defun previous-code-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (previous-buffer)
    (while
        (and
         (or (string-match-p "^\*" (buffer-name))
             ;;(string-match-p "dired-mode" (format "%s" (buffer-mode)) )
             )
         (not ( equal bread-crumb (buffer-name) )) )
      (previous-buffer))))
(global-set-key (kbd "C-<down>") 'previous-code-buffer)
;;(global-set-key (kbd "C-<down>") 'previous-buffer)

;; see diff
(global-set-key (kbd "<f8>") 'git-gutter+-next-hunk)
(global-set-key (kbd "S-<f8>") 'git-gutter+-previous-hunk)
(global-set-key (kbd "M-<f8>") 'git-gutter+-popup-hunk)
(global-set-key (kbd "C-x <f8>") 'git-gutter+-revert-hunk)

;; no trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default show-trailing-whitespace t)

;; no tabs, only spaces, and size 2 anyway
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; no wrap
(set-default 'truncate-lines t)
(global-set-key (kbd "<f3>") 'toggle-truncate-lines)

;; nice unique names for buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; backups
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; no autosave
(setq auto-save-default nil)

;;;
;; anything that may fail when landing on strange planets

;; be able to load packages
(setq package-enable-at-startup nil)
(package-initialize)

;; install every package to get the full pack
(defun install-all()
  (interactive)
  (package-refresh-contents)
  (ignore-errors (package-install 'git-gutter+))
  (ignore-errors (package-install 'popwin))
  (ignore-errors (package-install 'helm))
  (ignore-errors (package-install 'projectile))
  (ignore-errors (package-install 'helm-projectile))
  (ignore-errors (package-install 'auto-complete))
  (ignore-errors (package-install 'move-dup))
  (ignore-errors (package-install 'neotree))
  (ignore-errors (package-install 'web-mode))
  (ignore-errors (package-install 'scss-mode))
  (ignore-errors (package-install 'elixir-mode))
  (ignore-errors (package-install 'markdown-mode))
  (ignore-errors (package-install 'xclip))
  (ignore-errors (package-install 'undo-tree))
  (ignore-errors (package-install 'zoom-window))
  (ignore-errors (package-install 'popup-switcher))
  (ignore-errors (package-install 'ac-helm))
  ;; (ignore-errors (package-install 'ctags))
  ;; (ignore-errors (package-install 'auto-complete-exuberant-ctags))
  (message "All packages should be installed now")
)

(ignore-errors
  (add-hook 'markdown-mode-hook (lambda () (toggle-truncate-lines t)))
)

;; (ignore-errors
;;   (require 'ctags)
;;   (setq tags-revert-without-query t)
;;   (global-set-key (kbd "<f7>") 'ctags-create-or-update-tags-table)
;;   (global-set-key (kbd "M-.")  'ctags-search)
;;   (require 'auto-complete-exuberant-ctags)
;;   (ac-exuberant-ctags-setup)
;; )

(ignore-errors
  ;; https://github.com/nonsequitur/git-gutter-plus
  (global-git-gutter+-mode t)
  (global-set-key (kbd "C-x <f5>") 'git-gutter+-mode) ; Turn on/off in the current buffer
  (setq git-gutter+-lighter "")
)

(ignore-errors
  ;; https://github.com/m2ym/popwin-el
  (require 'popwin)
  (popwin-mode 1)
)

(ignore-errors
  ;; https://github.com/kostafey/popup-switcher
  (require 'popup-switcher)
  (global-set-key (kbd "M-q") 'psw-switch-buffer)
)

(ignore-errors
  (require 'neotree)
  (global-set-key (kbd "<f9>") 'neotree-toggle)
)

(ignore-errors
  ;; https://github.com/wyuenho/move-dup
  (require 'move-dup)
  (global-set-key (kbd "M-<up>") 'md/move-lines-up)
  (global-set-key (kbd "M-<down>") 'md/move-lines-down)
  (global-set-key (kbd "C-x C-<up>") 'md/duplicate-up)
  (global-set-key (kbd "C-x C-<down>") 'md/duplicate-down)
)

(ignore-errors
  (require 'undo-tree)
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "M-z") 'redo)
  (global-set-key (kbd "C-M-z") 'undo-tree-visualize)
  (setq undo-tree-mode-lighter "")
)

;; so slow on big files !!
;; (ignore-errors
;;   ;; https://github.com/zk-phi/indent-guide
;;   (require 'indent-guide)
;;   (setq indent-guide-char "|")
;;   (set-face-foreground 'indent-guide-face "color-234")
;;   (indent-guide-global-mode)
;; ;  (global-set-key (kbd "<f7>") 'indent-guide-mode)
;; )

(ignore-errors
  (require 'zoom-window)
  (global-set-key (kbd "C-x z") 'zoom-window-zoom)
  (setq zoom-window-mode-line-color "DarkGreen")
)

(ignore-errors
  ;; https://github.com/auto-complete/auto-complete
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
  (ac-config-default)
  ;; Add ac-source-words-in-all-buffer to ac-sources of all buffer
  (defun ac-common-setup ()
    (setq ac-sources (append ac-sources '(ac-source-words-in-all-buffer))))
  (ignore-errors (add-to-list 'ac-modes 'elixir-mode)) ;; add new modes to auto-complete
  (ignore-errors (add-to-list 'ac-modes 'markdown-mode))
  (ignore-errors (add-to-list 'ac-modes 'scss-mode))
  ;; clean mode-line
  (setq minor-mode-alist (assq-delete-all 'auto-complete-mode minor-mode-alist))
  (add-to-list 'minor-mode-alist '(auto-complete-mode ""))
)

(ignore-errors
  ;; http://web-mode.org/
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erubis\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-block-face nil)
  (setq web-mode-enable-part-face nil)
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "brightmagenta")
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "magenta")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "magenta")
  (ignore-errors (add-to-list 'ac-modes 'web-mode)) ;; add to auto-complete
)

(ignore-errors
  ;; https://emacs-helm.github.io/helm/
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (helm-adaptive-mode 1)
  (setq helm-completion-mode-string "")
)

(defun set-term-title()
  (interactive)
  (send-string-to-terminal (concat "\033]0; " (projectile-project-name) "\007"))
)

(defun custom-project-switch-action ()
  (interactive)
  (set-term-title)
  (neotree-dir (projectile-project-root))
  (neotree-hide)
  (projectile-find-file)
)

(ignore-errors
  ;; https://github.com/bbatsov/projectile
  (projectile-global-mode)
  ;; require helm when using projectile, a minimum
  (require 'helm-projectile)
  (helm-projectile-on)
  (global-set-key (kbd "C-p") 'helm-projectile-find-file)
  (global-set-key (kbd "C-M-p") 'helm-projectile-switch-project)
  (global-set-key (kbd "C-x C-g") 'helm-projectile-grep)
  (setq projectile-switch-project-action 'custom-project-switch-action)
  (setq projectile-mode-line (quote (:eval (format "[%s]" (projectile-project-name)))))
)

(ignore-errors
  (require 'ac-helm)
  (global-set-key (kbd "M-.") 'ac-complete-with-helm)
  (define-key ac-complete-mode-map (kbd "M-.") 'ac-complete-with-helm)
)

;; sessions
(desktop-save-mode 0) ;; deactivated if meant to be loaded manually

;; (defun setup-desktop ()
;;   "Setup desktop variables according to current project"
;;   (interactive)
;;   (shell-command "mkdir -p ~/.emacs.d/desktop") ;; default one
;;   (setq desktop-dirname             "~/.emacs.d/desktop/"
;;         desktop-base-file-name      "emacs.desktop"
;;         desktop-base-lock-name      "lock"
;;         desktop-path                (list desktop-dirname)
;;         desktop-save                t
;;         desktop-files-not-to-save   "^$" ;reload tramp paths
;;         desktop-load-locked-desktop nil)
;;   (ignore-errors  ;; try to setup everything if we have projectile working
;;     (shell-command (concat "mkdir -p ~/.emacs.d/desktop/" (projectile-project-name)))
;;     (setq desktop-dirname             (concat "~/.emacs.d/desktop/" (projectile-project-name) "/")
;;           desktop-path                (list desktop-dirname)))
;;   )

;; (defun load-desktop ()
;;   "Load the desktop for the current project and enable autosaving"
;;   (interactive)
;;   (let ((desktop-load-locked-desktop "ask"))
;;     (setup-desktop)
;;     (desktop-read)
;;     (desktop-save-mode 1)))

;; (defun save-desktop ()
;;   "Save the current desktop"
;;   (interactive)
;;   (setup-desktop)
;;   (desktop-save-in-desktop-dir))

;; (global-set-key (kbd "<f12>") 'load-desktop)
;; (global-set-key (kbd "<f24>") 'save-desktop)

(ignore-errors
  ;; xclip
  (xclip-mode 1)
)


;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ediff-current-diff-A ((t (:background "#330000"))))
;;  '(ediff-current-diff-B ((t (:background "#002200"))))
;;  '(ediff-current-diff-C ((t (:background "#222200"))))
;;  '(ediff-even-diff-A ((t (:background "color-234"))))
;;  '(ediff-even-diff-Ancestor ((t (:background "color-234"))))
;;  '(ediff-even-diff-B ((t (:background "color-234"))))
;;  '(ediff-even-diff-C ((t (:background "color-234"))))
;;  '(ediff-fine-diff-A ((t (:background "#220000"))))
;;  '(ediff-fine-diff-Ancestor ((t (:background "#002200"))))
;;  '(ediff-fine-diff-B ((t (:background "#003300"))))
;;  '(ediff-fine-diff-C ((t (:background "#444400"))))
;;  '(ediff-odd-diff-A ((t (:background "color-234"))))
;;  '(ediff-odd-diff-Ancestor ((t (:background "color-234"))))
;;  '(ediff-odd-diff-B ((t (:background "color-234"))))
;;  '(ediff-odd-diff-C ((t (:background "color-234"))))
;;  '(font-lock-comment-face ((t (:foreground "brightblack"))))
;;  '(font-lock-function-name-face ((t (:foreground "brightyellow" :weight bold))))
;;  '(font-lock-keyword-face ((t (:foreground "color-208" :weight bold))))
;;  '(font-lock-string-face ((t (:foreground "brightyellow"))))
;;  '(font-lock-type-face ((t (:foreground "color-172" :weight bold))))
;;  '(font-lock-variable-name-face ((t (:foreground "brightblue"))))
;;  '(highlight ((t (:background "color-233"))))
;;  '(isearch-fail ((t (:background "color-53"))))
;;  '(lazy-highlight ((t (:background "color-58"))))
;;  '(linum ((t (:background "black" :foreground "color-16"))))
;;  '(minibuffer-prompt ((t (:foreground "yellow"))))
;;  '(paren-face-match ((t (:background "color-240"))))
;;  '(show-paren-match ((t (:background "color-240"))))
;;  '(trailing-whitespace ((t (:background "black")))))

(when window-system
  ;; tweaks to make it fit on X

  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
