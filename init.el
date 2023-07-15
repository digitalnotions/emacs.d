;; init.el -- mwood's Emacs Initialization File

;; Author: Mark Wood <mark@markandkc.net>

;;; Commentary:
;;
;; This should startup Emacs with the correct theme and mainly orgmode configured in a way
;; that makes sense for my workflow
;;

;; Inspired by the following generous people who share their dotfiles:
;;   Mike Hamrick - https://gitlab.com/spudlyo/dotfiles/-/blob/master/emacs/.emacs.d/init.el
;; Temp stuff
;; (fset 'yes-or-no-p 'y-or-n-p)
;; (add-to-list 'default-frame-alist '(font . "InconsolataGo NF-12"))
;; (global-set-key (kbd "<S-return>") (quote toggle-frame-fullscreen))

;; ----------------------------------------
;; Startup Performance
;; ----------------------------------------
;; Make starup faster by reducing the frequecy of garbage collection
;; and then use a hook to measure startup time.

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 70 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; ----------------------------------------
;; Configure Identity
;; ----------------------------------------
(setq user-full-name "Mark Wood")
(setq user-mail-address "mark@digitalnotions.net")

;; ----------------------------------------
;; Basic Configuration
;; ----------------------------------------
;; Lets get some keybindings out of the way
(global-set-key (kbd "<f3>") 'find-file)
(global-set-key (kbd "<f4>") 'ivy-switch-buffer)
(global-set-key (kbd "<S-return>") (quote toggle-frame-fullscreen))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; And cleanup the window a bit
(setq inhibit-startup-message t)
(scroll-bar-mode -1)   ; Disable visible scrollbar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable tooltipsn
(set-fringe-mode 10)   ; Give some breathing room
(fset 'yes-or-no-p 'y-or-n-p)

;; Clarify that we don't want to be spammed by warnings
(setq warning-minimum-level :error)

;; Fix some weird keyboard stuff or MacOS
(when (eq system-type 'darwin)
  (global-set-key [home] 'move-beginning-of-line)
  (global-set-key [end] 'move-end-of-line))

;; Make dired actually list thing in an easy to read format
(when (eq system-type 'darwin)
  (setq dired-listing-switches "-laGh"))

;; Allow typing with selection to delete selction
(delete-selection-mode 1)

;; Fix the excessive scroll speed in windows
(when (eq system-type 'windows-nt)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta))((control) . text-scale)))
  (setq mouse-wheel-progressive-speed nil))

;; Avoid errors about the coding system by setting everything to UTF-8
(set-default-coding-systems 'utf-8)

;; ----------------------------------------
;; Package management
;; ----------------------------------------
;; Initialize package sources
(require 'package)

;; Identify package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package unless already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Let's configure sane defaults for use-package
(setq use-package-always-ensure t)

;; ----------------------------------------
;; Keep .emacs.d directory clean
;; ----------------------------------------
;; Set a base directory variable, and set it to nil to ensure we get an
;;   error if we don't specifically define it by environment.
(defvar mw/basedir nil
  "The base directory for all emacs config and org mode files")

;; On Mac OS, we want our base directory to be in our Syncthing folder ~/files/
(when (eq system-type 'darwin)
  (setq mw/basedir (expand-file-name "~/files/")))

(when (eq system-type 'gnu/linux)
  (setq mw/basedir (expand-file-name "~/files/")))

;; On Windows, we've already set the home directory as an environment variable
(when (eq system-type 'windows-nt)
  (setq mw/basedir (expand-file-name "~/")))

;; Now lets go to the basedir!
(when (eq system-type 'darwin)
  (cd mw/basedir))

;; Add .emacs.d to load path
(add-to-list 'load-path (expand-file-name ".emacs.d" mw/basedir))

;; Set directories off of the mw/basedir
(setq user-emacs-directory (expand-file-name ".cache/emacs" mw/basedir)
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common pahts to the new user-directory
(use-package no-littering)

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
	  (expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)


;; ----------------------------------------
;; Configure modeline
;; ----------------------------------------
;; Let fix the time display
(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

;; Enable mode diminishing by hiding pesky minor modes
(use-package diminish)

;; Turn on doom modeline and configure it
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (custom-set-faces
   '(mode-line ((t (:family "FiraMono Nerd Font" :height 0.8))))
   '(mode-line-inactive ((t (:family "FiraMono Nerd Font" :height 0.8)))))
  :custom
  (doom-modeline-height 1)
)

;; ----------------------------------------
;; Configure fonts and themes
;; ----------------------------------------
;; Set visible bell based on platform
;; (if (eq system-type 'darwin)
;;     (progn (message "Visible bell: Mac")
;; 	   (setq visible-bell nil))
;;   (progn (message "Visible bell: Not Mac")
;; 	 (setq visible-bell t)))

;; Set the font based on platform
(if (eq system-type 'darwin)
    ;; Configure MacOS Font
    (set-face-attribute 'default nil
			:font "FiraMono Nerd Font"
			:height 170)
  ;; Configure otherwise
  (set-face-attribute 'default nil
		      :font "FiraMono Nerd Font:antialias=subpixel"
		      :height 150))

;; (load-theme 'wombat)

;; Liking DOOM themes
;; Use the pale night theme, and turn on the doom visual bar
(use-package doom-themes :defer t)
(load-theme 'doom-palenight t)
(doom-themes-visual-bell-config)

(visual-line-mode 1)

;; ----------------------------------------
;; Configure spell checking
;; ----------------------------------------
;; We are going to use aspell for windows
(if (eq system-type 'windows-nt)
    (setq ispell-program-name "aspell"))

;; We want spell checking for all text files and for all comments and such
;; when programming
(defun flyspell-on-for-buffer-type ()
  "Enable flyspell appropriately for the major mode of the current buffer. Uses `flyspell-prog-mode` for modes derived from `prog-mode`, so only string and comments get checked. All other buffers use normal flyspell mode"
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
	(if (derived-mode-p 'prog-mode)
	    (progn
	      (message "Flyspell on (code)")
	      (flyspell-prog-mode))
	  ;; else
	  (progn
	    (message "Flyspell on (text)")
	    (flyspell-mode 1)))
	)))

(defun flyspell-toggle ()
  "Turn Flyspell on if it's off, or off if it's on. Use above function so that it's programmatically set correctly"
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
	(message "Flyspell off")
	(flyspell-mode -1))
    ;; else - Flyspell is off, turn it on
    (flyspell-on-for-buffer-type)))

;; Bind `flyspell-toggle` to a key
(global-set-key (kbd "C-c f") 'flyspell-toggle)

;; Now make sure that every new file attempts to have spell checking enabled
(add-hook 'find-file-hook 'flyspell-on-for-buffer-type)

;; ----------------------------------------
;; Attempt to sort out Markdown mode
;; ----------------------------------------
(defun mw/markdown-config ()
  (visual-line-mode 1))

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . mw/markdown-config)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 :map ivy-switch-buffer-map
	 ("C-d" . ivy-switch-buffer-kill))
  :init
  (ivy-mode 1))


(use-package org-roam
  :init
  ;; (setq org-roam-v2-ack t)
  ;; (when (eq system-type 'windows-nt)
  ;;   (setq org-roam-graph-executable "C:/Program Files/Graphviz/bin/dot.exe"))
  :custom
  (org-roam-directory (expand-file-name "org_roam" mw/basedir))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; (org-roam-setup)
  )

(defun mw/org-mode-setup ()
  (org-indent-mode)
  ;;  (variable-pitch-mode 1)
  ;;  (auto-fill-mode 0)
  ;; (linum-mode -1)
  (visual-line-mode 1)
  (diminish org-indent-mode)
  (abbrev-mode 1))

;; ----------------------------------------
;; Configure PDF Editing / Creation
;; ----------------------------------------
;;
(use-package pdf-tools
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install :no-query)
  (require 'pdf-occur))

;; ----------------------------------------
;; Configure Org Mode
;; ----------------------------------------
;; 
(use-package org
  :defer t
  :hook (org-mode . mw/org-mode-setup)
  :init
  ;; Configure base directory and agenda files
  (setq org-directory (expand-file-name "org_files/" mw/basedir))
  (setq org-default-notes-file (expand-file-name "refile.org" org-directory))
  (setq org-agenda-files (list org-default-notes-file
				(expand-file-name "work/" org-directory)
				(expand-file-name "personal/" org-directory)))
  ;; Lets setup our basic keybindings
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c o")
		  (lambda () (interactive) (find-file org-default-notes-file)))
  (global-set-key (kbd "<f11>") 'org-clock-goto)
  (global-set-key (kbd "<f12>") 'org-agenda)
  :config
  (require 'mw-org)
  (use-package org-superstar
    :after org
    :hook (org-mode . org-superstar-mode)
    :custom
    (org-superstar-remove-leading-stars t)
    (org-superstar-headline-hullets-list '("◉" "○" "●" "○" "●" "○" "●")))
  (set-face-attribute 'org-document-title nil :font "FiraMono Nerd Font" :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "FiraMono Nerd Font" :weight 'medium :height (cdr face)))
  ;;(require 'org-indent)
  (set-face-underline 'org-ellipsis nil)
  ;; This ends the use-package org-mode block
  )

;; ----------------------------------------
;; Define function to copy org-mode contents to a clipboard in markdown mode
;; ----------------------------------------
(defun mw/org-md-to-clipboard ()
  (interactive)
  (save-window-excursion
    (let ((org-export-with-toc nil))
      (let ((buf (org-export-to-buffer 'md "*tmp*" nil nil t t)))
	(save-excursion
	  (set-buffer buf)
	  (clipboard-kill-region (point-min) (point-max))
	  (kill-buffer-and-window)
	  )))))


;; ----------------------------------------
;; Define function for working with source blocks
;; ----------------------------------------
;; Enable syntax highlighting in source code blocks
(setq org-src-fontify-natively t)

;; ----------------------------------------
;; Configure Ox-Hugo
;; ----------------------------------------
;;  - This depends on org mode and allows Hugo blog authoring
(use-package ox-hugo
  :ensure t     ;; auto-install package form Melpa
  :pin melpa    ;; want to ensure we install form Melpa
  :after ox)



;; ----------------------------------------
;; Configure block templates
;; ----------------------------------------
;; These enable you to type things like <sh and tab to expand the template.

;; As of Org 9.2, require tempo explicitly
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Automatically clean whitespace
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
	 (prog-mode . ws-butler-mode)))

;; ----------------------------------------
;; Configure YAML mode
;; ----------------------------------------
;; Use YAML mode package and assign to .yml files
(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;; ----------------------------------------
;; Configure Magit
;; ----------------------------------------
;; Magit makes Git much easier in emacs
(use-package magit
  :ensure t
  :bind (("C-x g" . magit)))

;; ----------------------------------------
;; Configure Nix mode
;; ----------------------------------------
;; To provide syntax hightlighting in Nix files
(use-package nix-mode
  :mode "\\.nix\\'")


(provide 'init)
;;; init.el ends here
