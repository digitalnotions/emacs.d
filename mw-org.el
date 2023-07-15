;; org.el
;; --------------------------
;; Mark Wood
;; Created: 12/7/2016
;; Updated: 12/7/2016

;; Configure defaults for Org Mode

(require 'org-habit)
(require 'org-id)

(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-modules 'org-tempo)
(setq org-habit-graph-column 70)

(setq org-id-method (quote uuidgen))
(setq latex-run-command "pdftex")

(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)
(setq org-reverse-note-order nil)

(global-set-key (kbd "C-c j")
 		(lambda () (interactive) (find-file (expand-file-name "personal/journal.org.gpg" org-directory))))

(global-set-key (kbd "<f9> h") 'mw/hide-other)

(global-set-key (kbd "<f9> I") 'mw/punch-in)
(global-set-key (kbd "<f9> O") 'mw/punch-out)
(global-set-key (kbd "<f9> SPC") 'mw/clock-in-last-task)


(defun mw/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)))

  (setq org-ellipsis " ▾")
;;	org-hide-emphasis-markers t
;;	org-src-fontify-natively t
;;	org-fontify-quote-and-verse-blocks t
;;	org-hide-block-startup nil
;;	org-src-preserve-indentation nil
;;	org-startup-folded 'content
;;	org-cycle-separator-lines 2)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	      (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-repeat-to-state t)

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "orange red" :weight bold)
              ("NEXT" :foreground "cornflower blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "dark orange" :weight bold)
              ("HOLD" :foreground "violet red" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "DarkOrange1" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-log-done t)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

;; =============================================================================
;; SORT OUT TIMES AND DATES
;; =============================================================================
;; Make it so I can define alternative day abbreviations (thurs instead of thu)
(defvar parse-time-weekdays-longer
  '(("tues" . 2) ("thurs" . 4)))
(defvar parse-time-months-longer
  '(("sept" . 9)))

;; Don't show days:hours:minutes. Hours and minutes will suffice.
(setq org-duration-format (quote h:mm))

(eval-after-load 'parse-time
  '(progn
     (setq parse-time-weekdays (nconc parse-time-weekdays
				      parse-time-weekdays-longer))
     (setq parse-time-months (nconc parse-time-months
				    parse-time-months-longer))))

(setq org-agenda-timegrid-use-ampm t)

;; Make org intelligently specify AM/PM for dates between 1:00pm & 6:59 pm)

(defvar time-range-with-pm-suffix '("1:00" . "6:59"))

(defun org-analyze-date-dwim (original-fun ans org-def org-defdecode)
  (let* ((time (funcall original-fun ans org-def org-defdecode))
         (minute (nth 1 time))
         (hour (nth 2 time))
         (minutes (+ minute (* 60 hour)))
         s)
    (when (and (< hour 12)
               (not (string-match "am" ans))
               (>= minutes (org-hh:mm-string-to-minutes (car time-range-with-pm-suffix)))
               (<= minutes (org-hh:mm-string-to-minutes (cdr time-range-with-pm-suffix))))
      (setf (nth 2 time) (+ hour 12))
      (when (boundp 'org-end-time-was-given)
        (setq s org-end-time-was-given)
        (if (and s (string-match "^\\([0-9]+\\)\\(:[0-9]+\\)$" s))
            (setq org-end-time-was-given
                  (concat (number-to-string (+ 12 (string-to-number (match-string 1 s))))
                          (match-string 2 s))))))
    time))

(advice-add 'org-read-date-analyze :around #'org-analyze-date-dwim)

;; =============================================================================
;; CAPTURE ITEMS
;; =============================================================================
;; (when *win64*
;;   (print "Win64")
;;   (setq org-directory "/org/")
;;   (setq org-default-notes-file "/org/refile.org"))
;; (when *linux*
;;   (print "Linux")
;;   (setq org-directory "~/org/")
;;   (setq org-default-notes-file "~/org/refile.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file "refile.org")
	 "* TODO %?\n%U\n" :clock-in nil :clock-resume nil)
	("n" "Next Task" entry (file "refile.org")
	 "* NEXT %?\n%U\nDEADLINE: %t" :clock-in t :clock-resume t)
	("j" "Journal" entry (file+olp+datetree "personal/journal.org.gpg")
	 "* %<%I:%M %p>\n%?\n" :clock-in nil :clock-resume nil)
	("m" "Meeting" entry (file "refile.org")
	 "* MEETING MTG: %? :MEETING:\n%U" :clock-in t :clock-resume t)
	("a" "Appointment" entry (file "refile.org")
	 "* MEETING MTG: %? :MEETING:\n%^T\n%U" :clock-in nil :clock-resume nil)
	("p" "Phone call" entry (file "refile.org")
         "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
	("h" "Habit" entry (file "refile.org")
	 "* NEXT %?\n%U\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
	))

(defun mw/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

;;(add-hook 'org-clock-out-hook 'mw/remove-empty-drawer-on-clock-out 'append)

;; =============================================================================
;; REFILING ITEMS
;; =============================================================================
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)))

;; Use full outline paths for refile targets - filed directly with IDO
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use IDO for both buffer and file completion
;;(setq org-completeion-use-ido t)
;;(setq ido-everywhere t)
;;(ido-mode (quote both))

;; Use the current window when visiting files and buffers with ido
;;(setq ido-default-file-method 'selected-window)
;;(setq ido-default-buffer-method 'selected-window)

;; Use the current windows for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Exclude DONE state tasks from refile targets
(defun mw/verify-refile-target ()
  "Exclude todo keyword with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'mw/verify-refile-target)


;; =============================================================================
;; AGENDA CONFIGURATION
;; =============================================================================

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-span 'day)
(setq org-agenda-use-time-grid t)
(setq org-deadline-warning-days 5)
;; Show all future entries for repeating tasks
(setq org-agenda-show-future-repeats t)
;; Show all dates even if they are empty
(setq org-agenda-show-all-dates t)

;; Compact the block agenda view
(setq org-agenda-compact-blocks nil)
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("a" "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
		(tags-todo "-HOLD-CANCELLED/!"
			   ((org-agenda-overriding-header "Projects")
			    (org-agenda-skip-function 'mw/skip-non-projects)
			    (org-tags-match-list-sublevels 'indented)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "-CANCELLED/!NEXT"
			   ((org-agenda-overriding-header (concat "Project Next Tasks"
								  (if mw/hide-scheduled-and-waiting-next-tasks
								      ""
								    " (including WAITING and SCHEDULED tasks)")))
			    (org-agenda-skip-function 'mw/skip-projects-and-habits-and-single-tasks)
			    (org-tags-match-list-sublevels t)
			    (org-agenda-todo-ignore-scheduled mw/ignore-scheduled)
			    (org-agenda-todo-ignore-deadlines mw/ignore-deadlines)
			    (org-agenda-todo-ignore-with-date mw/hide-scheduled-and-waiting-next-tasks)
			    (org-agenda-sorting-strategy
			     '(todo-state-down effort-up category-keep))))
		(tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
			   ((org-agenda-overriding-header (concat "Project Subtasks"
								  (if mw/hide-scheduled-and-waiting-next-tasks
								      ""
								    " (including WAITING and SCHEDULED tasks)")))
			    (org-agenda-skip-function 'mw/skip-non-project-tasks)
			    (org-agenda-todo-ignore-scheduled mw/ignore-scheduled)
			    (org-agenda-todo-ignore-deadlines mw/ignore-deadlines)
			    (org-agenda-todo-ignore-with-date mw/hide-scheduled-and-waiting-next-tasks)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
			   ((org-agenda-overriding-header (concat "Standalone Tasks"
								  (if mw/hide-scheduled-and-waiting-next-tasks
								      ""
								    " (including WAITING and SCHEDULED tasks)")))
			    (org-agenda-skip-function 'mw/skip-project-tasks)
			    (org-agenda-todo-ignore-scheduled mw/ignore-scheduled)
			    (org-agenda-todo-ignore-deadlines mw/ignore-deadlines)
			    (org-agenda-todo-ignore-with-date mw/hide-scheduled-and-waiting-next-tasks)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "-CANCELLED+WAITING|HOLD/!"
			   ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
								  (if mw/hide-scheduled-and-waiting-next-tasks
								      ""
								    " (including WAITING and SCHEDULED tasks)")))
			    (org-agenda-skip-function 'mw/skip-non-tasks)
			    (org-tags-match-list-sublevels nil)
			    (org-agenda-todo-ignore-scheduled mw/ignore-scheduled)
			    (org-agenda-todo-ignore-deadlines mw/ignore-deadlines)))
		(tags-todo "-CANCELLED/!"
			   ((org-agenda-overriding-header "Stuck Projects")
			    (org-agenda-skip-function 'mw/skip-non-stuck-projects)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags "-REFILE/"
		      ((org-agenda-overriding-header "Tasks to Archive")
		       (org-agenda-skip-function 'mw/skip-non-archivable-tasks)
		       (org-tags-match-list-sublevels nil))))
	       nil))))

;; https://stackoverflow.com/questions/22394394/orgmode-a-report-of-tasks-that-are-done-within-the-week


;; Define R as the prefix key for reviewing time periods
(add-to-list 'org-agenda-custom-commands
             '("R" . "Review")
             )

;; comment settings for review
(setq my/org-agenda-review-settings
      '((org-agenda-files '("/org/work"
                            "/org"))
        (org-agenda-start-on-weekday 5)
        (org-agenda-show-all-dates t)
        (org-agenda-start-with-log-mode nil)
        (org-agenda-start-with-clockreport-mode t)
	(org-agenda-skip-scheduled-if-done t)
	(org-agenda-use-time-grid nil)
        (org-agenda-archives-mode t)
	(org-agenda-show-future-repeats nil)
        (org-agenda-hide-tags-regexp
         (concat org-agenda-hide-tags-regexp
                 "\\|ARCHIVE"))
        ))
        
;; Weekly review
;; Pay period review
(add-to-list 'org-agenda-custom-commands
             `("Rw" "Week in review"
               agenda ""
               ;; Agenda settings
               ,(append
                 my/org-agenda-review-settings
                 '((org-agenda-span 'week)
                   (org-agenda-overriding-header "Week In Review"))
                 )
               ("/org/review/weeklyreview.html")
               ))

;; Pay period review
(add-to-list 'org-agenda-custom-commands
             `("Rp" "Pay Period in review"
               agenda ""
               ;; Agenda settings
               ,(append
                 my/org-agenda-review-settings
                 '((org-agenda-span 14)
                   (org-agenda-overriding-header "Pay Period Review"))
                 )
               ("/org/review/payperiod.html")
               ))


;;(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done nil)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-time-grid
      '((daily today require-timed remove-match)
	(800 1000 1200 1400 1600 1800)
	"......"
	"---------------"
	))
;;(setq org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS")
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

;; Leave this many blank lines in colapsed view
(setq org-cycle-separator-lines 1
      ;; Take advantage of extra screen width
      org-agenda-tags-column 'auto
      ;; Always start the agenda on friday (to match work week)
      org-agenda-start-on-weekday 5
      ;; Only show 5 days of habit history
      org-habit-preceding-days 10
      ;; Only show the next two days for habits
      org-habit-following-days 5
      ;; Stop agenda from showing both the deadline and scheduled item
      org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
      ;; Honor todo-list options in a tags-todo search (agenda search)
      org-agenda-tags-todo-honor-ignore-options t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 2 :fileskip0 t :compact t :narrow 80 :formula %)))

;; Customize Clocktable
;; Fix leading level indicators to be arrows
(defun my/org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "^"))
      (while (> level 2)
	(setq level (1- level)
	      str (concat str "--")))
      (concat str "-> "))))

(advice-add 'org-clocktable-indent-string
	    :override #'my/org-clocktable-indent-string)

(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 4 :fileskip0 t :compact t :narrow 70 :score 0))

;; Agenda helper functions
;; Inspired / taken from https://github.com/gjstein/emacs.d

;; Search for a "===" and go to the next line
(defun gs/org-agenda-next-section ()
  "Go to next section in the org agenda buffer."
  (interactive)
  (if (search-forward "===" nil t 1)
      (forward-line 1)
    (goto-char (point-max)))
  (beginning-of-line))

;; search for "===" and go to previous line
(defun gs/org-agenda-prev-section ()
  "Go to next section in the org agenda buffer."
  (interactive)
  (if (search-forward "===" nil t -1)
      (forward-line 1)
    (goto-char (point-max))))

;; remove empty agenda blocks
(defun gs/remove-agenda-regions ()
  (save-excursion
    (goto-char (point-min))
    (let ((region-large t))
      (while (and (< (point) (point-max)) region-large)
	(set-mark (point))
	(gs/org-agenda-next-section)
	(if (< (- (region-end) (region-beginning)) 5) (setq region-large nil)
	  (if (< (count-lines (region-beginning) (region-end)) 4)
	      (delete-region (region-beginning) (region-end)))
	  )))))

(add-hook 'org-agenda-finalize-hook 'gs/remove-agenda-regions)


;;(setq org-agenda-custom-commands
;;      '(;; match those tagged with :unfiled:, are not scheduled, are not done
;;	("id" "[d]eadline not set" tags "-DEADLINE={.+}/!+TODO")))

;; =============================================================================
;; CLOCKING FUNCTIONS
;; =============================================================================
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

(setq org-clock-modeline-total 'today)

;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'mw/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;; Ensure that time editing uses discrete minute intervals
(setq org-time-stamp-rounding-minutes (quote (1 1)))

;; Ensure that orgmode always uses hours:minutes, instead of days when duration is greater than 24 hours
(setq org-time-clocksum-format
      (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "6:00"
	      :min-duration 0
	      :max-gap "0:00"
	      :gap-ok-around ("4:00"))))
			    

(setq mw/keep-clock-running nil)

(defun mw/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (mw/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (mw/is-project-p))
      "TODO"))))

(defun mw/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun mw/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq mw/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (mw/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (mw/clock-in-organization-task-as-default)))))

(defun mw/punch-out ()
  (interactive)
  (setq mw/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun mw/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun mw/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when mw/keep-clock-running
            (mw/clock-in-default-task)))))))

;; Create an ID by going to the task and running org-id-get-create

(defvar mw/organization-task-id "596E2385-F961-48B5-829E-6DCA6E89B839")

(defun mw/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find mw/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun mw/clock-out-maybe ()
  (when (and mw/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (mw/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'mw/clock-out-maybe 'append)

(defun mw/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(defun mw/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'mw/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'mw/mark-next-parent-tasks-todo 'append)

;; =============================================================================
;; ARCHIVE CONFIGURATION
;; =============================================================================
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun mw/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

;; =============================================================================
;; HELPER FUNCTIONS
;; =============================================================================
(defun mw/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun mw/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (mw/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun mw/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun mw/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun mw/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun mw/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar mw/hide-scheduled-and-waiting-next-tasks 1)
(defvar mw/ignore-scheduled 'all)
(defvar mw/ignore-deadlines 'near)

(defun mw/toggle-next-task-display ()
  (interactive)
  (setq mw/hide-scheduled-and-waiting-next-tasks (not mw/hide-scheduled-and-waiting-next-tasks))
  (if mw/hide-scheduled-and-waiting-next-tasks
      ;; ignore == true
      (setq mw/ignore-scheduled 'all
            mw/ignore-deadlines 'near)
    ;; ignore == false
    (setq mw/ignore-scheduled 'nil
          mw/ignore-deadlines 'nil))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if mw/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun mw/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (mw/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun mw/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (mw/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (mw/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun mw/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (mw/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((mw/is-project-p)
            nil)
           ((and (mw/is-project-subtree-p) (not (mw/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun mw/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((mw/is-task-p)
        nil)
       (t
        next-headline)))))

(defun mw/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((mw/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun mw/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and mw/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((mw/is-project-p)
        next-headline)
       ((and (mw/is-task-p) (not (mw/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun mw/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((mw/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (mw/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (mw/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun mw/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((mw/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((mw/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun mw/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((mw/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (mw/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT" "MEETING")))
        subtree-end)
       ((not (mw/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun mw/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((mw/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun mw/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (mw/is-subproject-p)
        nil
      next-headline)))

(defun mw/skip-tag (tag)
  "Skip entries with a specific TAG value"
  (let* ((entry-tags (org-get-tags-at (point))))
    (if (member tag entry-tags)
	(progn (outline-next-heading) (point))
      nil)))


(setq org-agenda-use-tag-inheritance '(search timeline aganda))

;; =============================================================================
;; BABEL CONFIGURATION
;; =============================================================================
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   ))

;; =============================================================================
;; LaTeX CONFIGURATION
;; =============================================================================

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
		 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'mw-org)
;;; org.el ends here
