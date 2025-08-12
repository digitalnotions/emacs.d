
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

;; ******************************************************************************
;; ORG MODE
;; ******************************************************************************

(use-package org
  :ensure t
  :demand t
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("<f12>" . org-agenda))
  ;;; Initialization
  :init
  (setq org-directory (expand-file-name "~/org" mw/basedir)
	org-default-notes-file (expand-file-name "agenda/refile.org" org-directory)
	org-agenda-files (list org-default-notes-file
			       (expand-file-name "agenda/" org-directory)))
  :config
  ;; ============================================
  ;; Begin Org Configuration
  ;; ============================================
  (which-key-add-keymap-based-replacements org-mode-map
    "C-c \""      "org-plot"
    "C-c C-v"     "org-babel"
    "C-c C-x"     "org-extra-commands")

  
  ;; Set fonts for org mode headings / titles
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 0.9)
		  (org-level-6 . 0.9)
		  (org-level-7 . 0.9)
		  (org-level-8 . 0.9)))
    (set-face-attribute (car face) nil :weight 'medium :height (cdr face)))
  (set-face-underline 'org-ellipsis nil)

  ;; Configure task logging and put into a drawer  
  (setq org-log-done 'time
	org-log-into-drawer t
	org-log-state-notes-insert-after-drawers nil)

  ;; Configure capture templates
  (setq org-capture-templates
	'(("t" "Todo" entry (file "refile.org")
	   "* TODO %?\n%U\n" :clock-in nil :clock-resume nil)
	  ("m" "Meeting" entry (file "refile.org")
	   "* MEETING MTG: %? :MEETING:\n%U" :clock-in nil :clock-resume nil)
	  )
	)

  ;; ============================================
  ;; End Org Configuration
  ;; ============================================
  )

(use-package org-roam
  :ensure t
  :demand t
  :after org
  :hook
  (org-capture-prepare-finalize . mw/org-capture-newline-at-end)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-roam-dailies-map
	 ("Y" . org-roam-dailies-capture-yesterday)
	 ("T" . org-roam-dailies-capture-tomorrow)
	 ("d" . mw/goto-todays-daily)
	 :map org-roam-node-map
	 ("S-<mouse-1>" . org-roam-node-visit)
	 :map org-roam-preview-map
	 ("S-<mouse-1>" . org-roam-node-visit))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :init
  (which-key-add-key-based-replacements
    "C-c n" "org-roam-commands"
    "C-c n d" "roam-dailies-cmds")
  ;; Define dailies filename and header
  (setq mw/daily-note-filename "%<%Y-%m-%d>.org"
	mw/daily-note-header "#+title: %<%Y-%m-%d>\n#+filetags: journal\n\n[[roam:%<%Y-%B>]]\n\nToday is a %<%A> and is day %<%j> of %<%Y>\n\n"
	mw/accomplishment-note-filename "Accomplishments-%<%Y>.org"
	mw/accomplishment-note-header "#+title: %<%Y> Accomplishments\n#+filetags: accomplishments\n\n")
  :custom
  (org-roam-directory (expand-file-name "roam" mw/basedir))
  (org-roam-dailies-directory "journal/")
  :config
    ;; Allow for more details in a vertical completion framework
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:30}" 'face 'org-tag)))
  ;; Define goto today with a default capture template
  (defun mw/goto-todays-daily ()
    "Goto today's daily note using the default template file name."
    (interactive)
    (org-roam-dailies-goto-today "d"))
  (defun mw/org-capture-newline-at-end ()
    (goto-char (point-max))
    (insert "\n"))
  (require 'org-roam-dailies) ;; ensure keymap is available
  (require 'org-roam-node)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
	`(("d" "default" plain
	   "%?"
	   :if-new (file+head ,"%<%Y%m%d%H%M%S>-${slug}.org"
			      ,"#+title: ${title}\n")
	   :unnarrowed t)
	  ("c" "customer" plain
	   "* Summary:\nARR: %^{ARR}\n Salesforce Link: %^{SFLink}\n\n* Notes:\n%?"
	   :if-new (file+head ,"customers/%<%Y%m%d%H%M%S>-${slug}.org"
			      ,"#+title: ${title}\n#+filetags: :gitlab:customer:")
	   :unnarrowed t)
	  ("y" "YouTube Video" plain
	   "\n* Metadata:\n - Video Link: %^{Video Link}\n\n* Notes:\n%?"
	   :if-new (file+head ,"%<%Y%m%d%H%M%S>-${slug}.org"
			      ,"#+title: ${title}\n#+filetags: :YouTube:")
	   :unnarrowed t)
	  ))
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
  (when (eq system-type 'darwin)
    (setq org-roam-graph-viewer "/Applications/Firefox.app/contents/MacOS/Firefox"))

  (setq	org-roam-graph-executable "dot"
	org-roam-graph-link-hidden-types '("file" "https"))
  (org-roam-db-autosync-mode)
  (org-roam-setup)) ;; Initialize Org-Roam at startup)


(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-super-agenda
  :ensure t
  :hook (org-agenda-mode-hook . org-super-agenda-mode))

(use-package consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Active the minor mode
  (consult-org-roam-mode 1)
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c n f" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search)
  :custom
  ;; Use ripgrep for searching with consult-org-roam-search
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  )


(provide 'org-config)
