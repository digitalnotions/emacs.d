;; init.el -- Emacs Initialization File

;; Author: Mark Wood <mark@digitalnotions.net>

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
;; Set the startup to max positive number to reduce garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; After initialization, set garbage threshold back to a more rational number
(add-hook 'after-init-hook
	  `(lambda ()
	     (setq gc-cons-threshold (* 70 1000 1000)))
	  t)

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
(global-set-key (kbd "<f4>") 'ido-switch-buffer)
;;(global-set-key (kbd "<S-return>") (quote toggle-frame-fullscreen))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ----------------------------------------
;; Cleanup display
;; ----------------------------------------
(setq inhibit-startup-message t     ; I don't need to see the startup message
      warning-minimum-level :error  ; Don't show warnings all the time
      native-comp-async-report-warnings-errors nil) ; Don't interrupt with native compilation warnings
(scroll-bar-mode -1)                ; Disable visible scrollbar
(tool-bar-mode -1)                  ; Disable the toolbar
(tooltip-mode -1)                   ; Disable tooltipsn
(set-fringe-mode 10)                ; Give some breathing room
(fset 'yes-or-no-p 'y-or-n-p)       ; I don't want to type "yes" or "no" out fully
(visual-line-mode 1)                ; Enable visual line mode globally


;; ----------------------------------------
;; Fix MacOS Quirks
;; ----------------------------------------
(when (eq system-type 'darwin)
  ;; Fix some weird keyboard stuff or MacOS
  (global-set-key [home] 'move-beginning-of-line)
  (global-set-key [end] 'move-end-of-line)
  ;; On MacOS, use Firefox to open links
  (setq browse-url-browser-function #'browse-url-firefox)
  ;; Make dired actually list things in an easy to read format
  (setq dired-listing-switches "-laGh"
	dired-use-ls-dired nil))


;; ----------------------------------------
;; Basic preferences
;; ----------------------------------------
;; Allow typing with selection to delete selction
(delete-selection-mode 1)
;; Avoid errors about the coding system by setting everything to UTF-8
(set-default-coding-systems 'utf-8)

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
	  (expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)


;; ----------------------------------------
;; Configure package management
;; ----------------------------------------
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Refresh package archives (GNU Elpa)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package unless already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)


;; ----------------------------------------
;; Keep .emacs.d directory clean
;; ----------------------------------------
;; Set a base directory variable, and set it to nil to ensure we get an
;;   error if we don't specifically define it by environment.
(defvar mw/basedir nil
  "The base directory for all emacs config")
(defvar mw/orgdir nil
  "The base directory for all org-mode files")

;; On Mac OS, we want our base directory to be in our Syncthing folder ~/files/
(when (eq system-type 'darwin)
  (setq mw/basedir (expand-file-name "~/files/"))
  (setq mw/orgdir (expand-file-name "~/files/"))
  (cd mw/orgdir))

(when (eq system-type 'gnu/linux)
  (setq mw/basedir (expand-file-name "~/"))
  (setq mw/orgdir (expand-file-name "~/files/"))
  (cd mw/orgdir))

;; Add .emacs.d to load path
(add-to-list 'load-path (expand-file-name ".emacs.d" mw/basedir))

;; Set directories off of the mw/basedir
(setq user-emacs-directory (expand-file-name ".cache/emacs" mw/basedir)
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      ;; Setup backups
      backup-directory-alist '(("." . "~/.config/emacs/backups"))
      delete-old-versions t
      version-control t
      vc-make-backup-files t
      auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
	  (expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))


;; ----------------------------------------
;; Configure modeline
;; ----------------------------------------
;; Let fix the time display
(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

;; Enable mode diminishing by hiding pesky minor modes
(use-package diminish
  :ensure t)

;; Turn on doom modeline and configure it
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (if (eq system-type 'darwin)
      (custom-set-faces
       '(mode-line ((t (:family "Fira Mono" :height 0.9))))
       '(mode-line-inactive ((t (:family "Fira Mono" :height 0.9)))))
    (custom-set-faces
     '(mode-line ((t (:family "FiraMono Nerd Font" :height 0.9))))
     '(mode-line-inactive ((t (:family "FiraMono Nerd Font" :height 0.9))))))
    
  :custom
  (setq doom-modeline-height 35)
)


;; ----------------------------------------
;; Configure fonts and themes
;; ----------------------------------------
;; Set the font based on platform 
(if (eq system-type 'darwin)
    ;; Configure MacOS Font 
    (set-face-attribute 'default nil
			:font "Fira Mono"
			:height 190)
  ;; Configure otherwise
  (set-face-attribute 'default nil
		      :font "FiraMono Nerd Font:antialias=subpixel"
		      :height 150))

;; Let's make the window a decent size if we're on high resolution
;; This must be done after fonts are configured so that the
;; value of 'frame-char-height' is correct.
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (when (display-graphic-p)
      (progn
	;; Go for a 160 wide window if we have enough space on the display
	(if (> (x-display-pixel-width) 1280)
	    (add-to-list 'default-frame-alist (cons 'width 160))
	  (add-to-list 'default-frame-alist (cons 'width 80)))
	;; For height, subtract a bit off the screen height
	;; and go with that
	(let ((monheight (nth 4 (assq 'workarea (car (display-monitor-attributes-list)))))
	      (monwidth (nth 3 (assq 'workarea (car (display-monitor-attributes-list))))))
	  (add-to-list 'default-frame-alist
		       (cons 'height (/ (- monheight 200)
					(frame-char-height))))
	  (add-to-list 'default-frame-alist
		       (cons 'top 100))
	  (message "monheight %d" monheight)
	  (message "subtract %d" (- monheight 200))
	  (message "frame-char-height %d" (frame-char-height))
	  (message "height %d" (/ (- monheight 200)
				  (frame-char-height))))
	)))

(set-frame-size-according-to-resolution)

;; Use the Pale Night theme from Doom themes, and turn on the doom visual bar
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  ;;  (load-theme 'doom-palenight t)
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
)

;; ----------------------------------------
;; Configure spell checking
;; ----------------------------------------
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
;; Configure competions
;; ----------------------------------------
;; References:
;; - https://github.com/jwiegley/dot-emacs/blob/master/init.org#completions
(use-package vertico
  :ensure t
  :init

  (use-package orderless
    :ensure t
    :commands (orderless)
    :custom (completion-styles '(orderless flex)))

  (use-package marginalia
    :ensure t
    :bind (:map minibuffer-local-map
           ("M-A" . marginalia-cycle))
    :custom
    (marginalia-annotators
     '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))

  (vertico-mode t)

  :config
  ;; Do not allow the cursor in the minibuffer prompt
;;  (setq minibuffer-prompt-properties
;;	'(read-only t cursor-intangible t face minibuffer-prompt))
;;  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package vertico-directory
  :ensure nil    ; Don't need to ensure as it comes with vertico
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char) ; more like ido
	      ("C-DEL" . vertico-directory-delete-char))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))



;; (use-package corfu
;;   :ensure t
;;   :custom
;;   (corfu-cycle t)                      ; Allows cycling through candidates
;;   (corfu-auto t)                       ; Enable auto completion
;;   (corfu-auto-prefix 2)                ; Enable auto completion
;;   (corfu-auto-delay 0.2)               ; Enable auto completion
;;   (corfu-popupinfo-delay '(0.5 . 0.2))
;;   (corfu-quit-at-boundary 'separator)
;;   (corfu-preview-current 'insert)      ; Do not preview current candidate
;;   (corfu-preselect-first nil)

;;   :bind (:map corfu-map
;;          ("M-SPC"      . corfu-insert-separator)
;;          ;; ("RET"        . nil)
;;          ;; ("<escape>"   . corfu-quit)
;;          ;; ("S-<return>" . corfu-insert)
;;          ;; ("TAB"        . corfu-next)
;;          ;; ([tab]        . corfu-next)
;;          ;; ("S-TAB"      . corfu-prevous)
;; 	 ;; ([backtab]    . corfu-previous)
;; 	 )

;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode)
;;   (corfu-popupinfo-mode)

;;   :config
;;   (add-hook 'eshell-mode-hook
;; 	    (lambda () (setq-local corfu-quit-at-boundary t
;; 				   corfu-quit-no-match t
;; 				   corfu-auto nil)
;; 	      (corfu-mode))))

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

;; ----------------------------------------
;; Show keybindings after a pause
;; ----------------------------------------
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; ----------------------------------------
;; Configure PDF Editing / Creation
;; ----------------------------------------
;;
(use-package pdf-tools
  :ensure t
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install :no-query)
  (require 'pdf-occur))

;; ----------------------------------------
;; Configure Org-Roam 
;; ----------------------------------------
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (expand-file-name "org_roam" mw/orgdir))
  (org-roam-dailies-directory "journal/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
 	 :map org-roam-dailies-map
	 ("Y" . org-roam-dailies-capture-yesterday)
	 ("T" . org-roam-dailies-capture-tomorrow)
  	 ("d" . mw/goto-todays-daily))

  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :init
  ;; Define dailies filename and header
  (setq mw/daily-note-filename "%<%Y-%m-%d>.org"
	mw/daily-note-header "#+title: %<%Y-%m-%d>\n#+filetags: Journal\n\n[[roam:%<%Y-%B>]]\n\nToday is a %<%A> and is day %<%j> of %<%Y>\n\n"
	mw/accomplishment-note-filename "Accomplishments-%<%Y>.org"
	mw/accomplishment-note-header "#+title: %<%Y> Accomplishments\n#+filetags: accomplishments\n\n")
  :config
  (require 'org-roam-dailies) ;; ensure keymap is available
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode)
  ;; Define goto today with a default capture template
  (defun mw/goto-todays-daily ()
    "Goto today's daily note using the default template file name."
    (interactive)
    (org-roam-dailies-goto-today "d"))
  ;; Define org-roam daily templates
  (setq org-roam-dailies-capture-templates
	`(("d" "default" entry
	   "* %?"
	   :if-new (file+head ,mw/daily-note-filename
			      ,mw/daily-note-header)
	   :empty-lines-before 1)
	  ("l" "log entry" entry
	   "* %<%H:%M> - %?"
	   :if-new (file+head+olp ,mw/daily-note-filename
				  ,mw/daily-note-header
				  ("Log"))
	   :empty-lines-before 1)
	  ("j" "journal" entry
	   "* %<%H:%M> - Journal  :journal:\n\n%?\n\n"
	   :if-new (file+head+olp ,mw/daily-note-filename
				  ,mw/daily-note-header
				  ("Journal"))
	   :empty-lines-before 1)
	  ("m" "meeting" entry
	   "* %<%H:%M> - %^{Meeting Title} (%<%d-%b-%Y>) :meetings:\n\n%?\n\n"
	   :if-new (file+head+olp ,mw/daily-note-filename
				  ,mw/daily-note-header
				  ("Meetings"))
	   :empty-lines-before 1)
	  ("a" "accomplishment" entry
	   "* %<%B %e> - %^{Accomplishment Description}\n\n%?\n\n"
	   :if-new (file+head ,mw/accomplishment-note-filename
			      ,mw/accomplishment-note-header)
	   :empty-lines-before 1
	   :prepend t)

	  ))

  )

;; ----------------------------------------
;; Configure Org Mode
;; ----------------------------------------
(defun mw/org-mode-setup ()
  (org-indent-mode)
  ;;  (variable-pitch-mode 1)
  ;;  (auto-fill-mode 0)
  ;; (linum-mode -1)
  (visual-line-mode 1)
  (diminish org-indent-mode)
  (abbrev-mode 1))

(use-package org
  :ensure t
  :defer t
  :hook (org-mode . mw/org-mode-setup)
  :init
  ;; Configure base directory and agenda files
  (setq org-directory (expand-file-name "org_files/" mw/orgdir))
  (setq org-default-notes-file (expand-file-name "refile.org" org-directory))
  (setq org-agenda-files (list org-default-notes-file
			       (expand-file-name "work/" org-directory)
			       (expand-file-name "work/notes/" org-directory)
			       (expand-file-name "personal/" org-directory)))
  (setq org-src-fontify-natively t)
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
    :ensure t
    :after org
    :hook (org-mode . org-superstar-mode)
    :custom
    (org-superstar-remove-leading-stars t)
    (org-superstar-headline-hullets-list '("◉" "○" "●" "○" "●" "○" "●")))

  (if (eq system-type 'darwin)
      ;; For MacOS
      (set-face-attribute 'org-document-title nil
			  :font "Fira Mono"
			  :weight 'bold
			  :height 1.3)
    ;; Else
    (set-face-attribute 'org-document-title nil
			:font "FiraMono Nerd Font"
			:weight 'bold
			:height 1.3))
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (if (eq system-type 'darwin)
	(set-face-attribute (car face) nil
			    :font "Fira Mono"
			    :weight 'medium :height (cdr face))
      	(set-face-attribute (car face) nil
			    :font "FiraMono Nerd Font"
			    :weight 'medium :height (cdr face))))
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
;; Configure Ox-Hugo
;; ----------------------------------------
;;  - This depends on org mode and allows Hugo blog authoring
(use-package ox-hugo
  :ensure t     ;; auto-install package form Melpa
  :pin melpa    ;; want to ensure we install form Melpa
  :after ox)

;; ----------------------------------------
;; For the love of everything, do your best to never split my window vertically
;; ----------------------------------------

;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
;; (with-eval-after-load "window"
;;   (defcustom split-window-below nil
;;     "If non-nil, vertical splits produce new windows below."
;;     :group 'windows
;;     :type 'boolean)

;;   (defcustom split-window-right nil
;;     "If non-nil, horizontal splits produce new windows to the right."
;;     :group 'windows
;;     :type 'boolean)

;;   (fmakunbound #'split-window-sensibly)

;;   (defun split-window-sensibly
;;       (&optional window)
;;     (setq window (or window (selected-window)))
;;     (or (and (window-splittable-p window t)
;;              ;; Split window horizontally.
;;              (split-window window nil (if split-window-right 'left  'right)))
;;         (and (window-splittable-p window)
;;              ;; Split window vertically.
;;              (split-window window nil (if split-window-below 'above 'below)))
;;         (and (eq window (frame-root-window (window-frame window)))
;;              (not (window-minibuffer-p window))
;;              ;; If WINDOW is the only window on its frame and is not the
;;              ;; minibuffer window, try to split it horizontally disregarding the
;;              ;; value of `split-width-threshold'.
;;              (let ((split-width-threshold 0))
;;                (when (window-splittable-p window t)
;;                  (split-window window nil (if split-window-right
;;                                               'left
;;                                             'right))))))))

;; (setq-default split-height-threshold  4
;;               split-width-threshold   120) ; the reasonable limit for horizontal splits

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(provide 'init)
;;; init.el ends here
