;; init.el -- mwood's Emacs Initialization File

;; Author: Mark Wood <mark@markandkc.net>

;;; Commentary:
;;
;; This should startup Emacs with the correct theme and mainly orgmode configured in a way
;; that makes sense for my workflow

;; ----------------------------------------
;; Startup Performance
;; ----------------------------------------
;; Make starup faster by reducing the frequency of garbage collection
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
(global-set-key (kbd "<f4>") 'switch-to-buffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; And cleanup the window a bit
(setq inhibit-startup-message t)
(scroll-bar-mode -1)   ; Disable visible scrollbar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable tool tips
(set-fringe-mode 10)   ; Give some breathing room
(fset 'yes-or-no-p 'y-or-n-p)

;; Fix some weird keyboard stuff or MacOS
(when (eq system-type 'darwin)
  (global-set-key [home] 'move-beginning-of-line)
  (global-set-key [end] 'move-end-of-line))

;; Make dired actually list thing in an easy to read format
(when (eq system-type 'darwin)
  (setq dired-listing-switches "-laGh"
	dired-use-ls-dired nil))

;; Allow typing with selection to delete selction
(delete-selection-mode 1)

;; Fix the excessive scroll speed in windows
(when (eq system-type 'windows-nt)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta))((control) . text-scale)))
  (setq mouse-wheel-progressive-speed nil))

;; Avoid errors about the coding system by setting everything to UTF-8
(set-default-coding-systems 'utf-8)

;; Make all files use visual line mode
(global-visual-line-mode 1)

;; ----------------------------------------
;; Keep .emacs.d directory clean
;; ----------------------------------------
;; Set a base directory variable, and set it to nil to ensure we get an
;;   error if we don't specifically define it by environment.
(defvar mw/basedir nil
  "The base directory for all emacs config and org mode files")

;; On Mac OS, we want our base directory to be in our Syncthing folder ~/files/
(when (eq system-type 'darwin)
  (setq mw/basedir (expand-file-name "~/org/")))

(when (eq system-type 'gnu/linux)
  (setq mw/basedir (expand-file-name "~/files/org/")))

;; Now lets go to the basedir!
(when (eq system-type 'darwin)
  (cd mw/basedir))

(when (eq system-type 'darwin)
  (setq mw/config-dir (expand-file-name "~/new_emacs/")))

(when (eq system-type 'gnu/linux)
  (setq mw/config-dir (expand-file-name "~/.emacs.d/")))

;; Add .emacs.d to load path
(add-to-list 'load-path mw/config-dir)

;; Set directories off of the mw/config-dir
(setq user-emacs-directory (expand-file-name ".cache/emacs" mw/config-dir)
      url-history-file (expand-file-name "url/history" user-emacs-directory))


;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
	  (expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; ----------------------------------------
;; Package management
;; ----------------------------------------
;; Initialize package sources
(require 'package)

;; Identify package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package unless already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; For time profile
(setq use-package-compute-statistics t)

;; ----------------------------------------
;; Setup trash
;; ----------------------------------------
(when (eq system-type 'darwin)
  (use-package osx-trash
    :ensure t
    :init (osx-trash-setup)))
(setq delete-by-moving-to-trash t)

;; ----------------------------------------
;; Fix appearance with font, theme, modeline, etc...
;; ----------------------------------------
(use-package all-the-icons
  :ensure t)

(if (eq system-type 'darwin)
    ;; Configure MacOS font
    (set-face-attribute 'default nil
			:font "FiraCode Nerd Font Mono"
			:height 170)
  ;; Configure otherwise
  (set-face-attribute 'default nil
		      :font "FiraMono Nerd Font:antialias=subpixel"
		      :height 150))

(use-package battery
  :ensure nil 
  :config
  (setq display-battery-mode t))

(use-package doom-themes
  :ensure t
  :config
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-palenight t)  
  ;; (load-theme 'doom-city-lights t)
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config)
  )

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (if (eq system-type 'darwin)
      (custom-set-faces
       '(mode-line ((t (:family "FiraCode Nerd Font Mono" :height 1.0))))
       '(mode-line-inactive ((t (:family "FiraCode Nerd Font Mono" :height 1.0)))))
    (custom-set-faces
     '(mode-line ((t (:family "FiraMono Nerd Font" :height 0.9))))
     '(mode-line-inactive ((t (:family "FiraMono Nerd Font" :height 0.9))))))
  :custom
  (doom-modeline-height 32)
  )

;; Add which-key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-right)
  (setq which-key-idle-delay 0.3)
  ;; Configure a bunch of replacement key descriptions
  ;; - https://gist.github.com/mmarshall540/a12f95ab25b1941244c759b1da24296d
  (which-key-add-key-based-replacements
    "C-c"           "mode-and-user"
    "C-h 4"         "help-other-win"
    "C-h"           "help"
    "C-x 4"         "other-window"
    "C-x 5"         "other-frame"
    "C-x 6"         "2-column"
    "C-x 8 '"       "´-ÁĆÉÍŃÓŚÚÝŹ"
    "C-x 8 * SPC"   "nb-space"
    "C-x 8 *"       "punct-curr"
    "C-x 8 ,"       "„-‚-¸-ĄÇŅŞ"
    "C-x 8 -"       "soft-hyphen"
    "C-x 8 ."       "·-ż"
    "C-x 8 /"       "÷-≠-ÅÆŁØ"
    "C-x 8 1 /"     "½-¼"
    "C-x 8 1"       "†-½-¼"
    "C-x 8 2"       "‡"
    "C-x 8 3 /"     "¾"
    "C-x 8 3"       "¾"
    "C-x 8 = /"     "Ǣ-ǣ"
    "C-x 8 ="       "¯-ĀĒḠĪŌŪȲǢ"
    "C-x 8 A"       "Æ"
    "C-x 8 N"       "№"
    "C-x 8 O"       "Œ-œ"
    "C-x 8 SPC"     "nb-space"
    "C-x 8 \""      "¨-ÄËÏÖẞÜÿ"
    "C-x 8 ^ ^"     "caron"
    "C-x 8 ^"       "sup-circ-caron"
    "C-x 8 _ H"     "nb-hyphen"
    "C-x 8 _ f"     "fig-dash"
    "C-x 8 _ h"     "hyphen"
    "C-x 8 _ m"     "em-dash"
    "C-x 8 _ n"     "en-dash"
    "C-x 8 _ q"     "horiz-bar"
    "C-x 8 _ −"     "minus"
    "C-x 8 _"       "sub-dash-≤≥ªº"
    "C-x 8 `"       "ÀÈÌÒÙ"
    "C-x 8 a"       "←-↔-→-æ"
    "C-x 8 e"       "emoji"
    "C-x 8 ~"       "≈-ÃÐÑÕÞ-¬"
    "C-x 8"         "insert-special"
    "C-x C-k C-q"   "kmacro-counters"
    "C-x C-k C-r a" "kmacro-add"
    "C-x C-k C-r"   "kmacro-register"
    "C-x C-k"       "keyboard-macros"
    "C-x RET"       "encoding/input"
    "C-x a i"       "abbrevs-inverse-add"
    "C-x a"         "abbrevs"
    "C-x n"         "narrowing"
    "C-x p"         "projects"
    "C-x r"         "reg/rect/bkmks"
    "C-x t ^"       "tab-bar-detach"
    "C-x t"         "tab-bar"
    "C-x v M"       "vc-mergebase"
    "C-x v b"       "vc-branch"
    "C-x v"         "version-control"
    "C-x w ^"       "window-detach"
    "C-x w"         "window-extras"
    "C-x x"         "buffer-extras"
    "C-x"           "extra-commands"
    "M-g"           "goto-map"
    "M-s h"         "search-highlight"
    "M-s"           "search-map")
  (when (version<= "31" emacs-version)
    (which-key-add-key-based-replacements
      "C-x p C-x" "projects-extra"
      "C-x w f"   "window-layout-flip"
      "C-x w o"   "rotate-windows"
      "C-x w r"   "window-layout-rotate"))
  )

;; ----------------------------------------
;; Fix the Huge Monitor vs. Tiny Window issue
;; ----------------------------------------
;; This must be done after fonts are configured so that the
;; value of 'frame-char-height' is correct.
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (when (display-graphic-p)
      (progn
	;; Go for a 160 wide window if we have enough space on the display
	(if (> (x-display-pixel-width) 1280)
	    (add-to-list 'default-frame-alist (cons 'width 140))
	  (add-to-list 'default-frame-alist (cons 'width 80)))
	;; For height, subtract a bit off the screen height
	;; and go with that
	(let ((monheight (nth 4 (assq 'workarea (car (display-monitor-attributes-list)))))
	      (monwidth (nth 3 (assq 'workarea (car (display-monitor-attributes-list))))))
	  (add-to-list 'default-frame-alist
		       (cons 'height (/ (- monheight 150)
					(frame-char-height))))
	  (add-to-list 'default-frame-alist
		       (cons 'top 75))
	  (message "monheight %d" monheight)
	  (message "subtract %d" (- monheight 150))
	  (message "frame-char-height %d" (frame-char-height))
	  (message "height %d" (/ (- monheight 150)
				  (frame-char-height))))
	)))

(set-frame-size-according-to-resolution)


;; ----------------------------------------
;; Figure out completion system
;; ----------------------------------------

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :bind (:map vertico-map
	      ("<escape>" . minibuffer-keyboard-quit))
  :init
  (vertico-mode)
  :config
  (setq enable-recursive-minibuffers t))

;; Persist history over Emacs restarts. Vertico sorts by history position
(use-package savehist
  :init
  (savehist-mode))

;; Orderless for minibuffer completion assistance
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia annotators-heavy marginalia-annotators-light nil))
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the modeline of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("C-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . verico-directory-tidy))

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package flyspell
  :ensure t
  :defer 1
  :bind (:map flyspell-mode-map ("C-:" . flyspell-buffer))
  :custom-face
  (flyspell-incorrect ((t (:underline (:color "red" :style wave :position nil)))))
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (flyspell-abbrev-p t))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package abbrev
  :defer 1
  :custom
  (abbrev-file-name (expand-file-name ".abbrev_defs" mw/config-dir))
  (abbrev-mode 1)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


(require 'org-config)

;; Sort out some custom functions
(defun mw/delete-current-file ()
  "Move the file current buffer is visiting to trash.
It also kills the current buffer."
  (interactive)
  ;; Ensure `delete-by-moving-to-trash` is non-nil so that `delete-file`
  ;; moves the file to the trash even if the customization is incorrect.
  (let ((delete-by-moving-to-trash t))
    (delete-file (buffer-file-name) t)
    (kill-current-buffer)))

(provide 'init)
;;; init.el ends here
