;;; init.el --- Initialisation file for Emacs

;; Copyright © 2020-2022 Egor Tolbaev <egor05.09.97@gmail.com>

;; Author: Egor Tolbaev <egor05.09.97@gmail.com>
;; URL: https://github.com/EgorTolbaev/.emacs.d
;; Keywords: emacs, init, init.el, dotfiles

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; I decided to create my own GNU Emacs configuration
;; to simplify my daily life by adding scripts and useful features.
;; This configuration does not claim to be ideal and is in constant development, but it is fully operational.
;; This file is generated based on myconfig.org

;;; Code:

(setq gc-cons-threshold (* 50 1000 1000))

(defun system-is-linux()
  (string-equal system-type "gnu/linux"))
(defun system-is-windows()
  (string-equal system-type "windows-nt"))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq inhibit-startup-message t)

;; (setq ring-bell-function 'ignore)              ; Отключить звуковой сигнал
(blink-cursor-mode -1)                            ; Курсор не мигает
(fset 'yes-or-no-p 'y-or-n-p)                     ; Замена yes-or-on на y-or-n

(menu-bar-mode -1)    ; Не показывать меню
(scroll-bar-mode -1)  ; Не показывать полосу прокрутки
(tool-bar-mode -1)    ; Не показывать панель инструментов

(setq visible-bell t) ; Установите видимый звонок

(setq user-full-name "Egor Tolbaev")              ; Имя пользователя
(setq user-mail-address "egor05.09.97@gmail.com") ; Email пользователя

;; Показ номера строки стандартными средствами
(column-number-mode)
(global-display-line-numbers-mode t)

;; Отключить номера строк где они не нужны
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                org-agenda-mode-hook
                treemacs-mode-hook
                eww-mode-hook
                calendar-mode-hook
                deft-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defvar et/default-font-size 110)
;;(set-face-attribute 'default nil :font "Consolas")
(set-face-attribute 'default nil :font "Source Code Pro Medium" :height et/default-font-size)
;(set-fontset-font t 'latin "Noto Sans")
(set-fontset-font t 'latin "Cantarell")

(set-language-environment 'UTF-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read    'utf-8)
(setq file-name-coding-system           'utf-8)
(set-keyboard-coding-system        'utf-8-unix)
(set-terminal-coding-system             'utf-8)
(prefer-coding-system                   'utf-8)

(electric-pair-mode t)
(show-paren-mode 1)

(setq word-wrap t)
(global-visual-line-mode t)

(add-hook 'before-save-hook '(lambda () (delete-trailing-whitespace)))

(setq make-backup-files nil)        ; Не нужны файлы резервных копий
(setq auto-save-list-file-name nil) ; Не нужны файлы .saves
(setq auto-save-default nil)        ; Не хочу автосохранения

(setq auto-mode-alist
    (append
     '(("\\.el$"  . emacs-lisp-mode)
       ("\\.org$" . org-mode)
       ("\\.tex$" . latex-mode))))

(defun edit-configs ()
  "Opens the README.org file."
  (interactive)
  (find-file "~/.emacs.d/myconfig.org"))

(global-set-key (kbd "C-x e") #'edit-configs)

(use-package doom-themes
  :config
    (load-theme 'doom-Iosvkem))

(defun set-light-theme()
  (interactive)
  (load-theme 'doom-opera-light))
  (global-set-key (kbd "<f8>") 'set-light-theme)

(defun set-night-theme()
  (interactive)
  (load-theme 'doom-Iosvkem))
  (global-set-key (kbd "<f9>") 'set-night-theme)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons)

(setq display-time-24hr-format t) ; 24-часовой временной формат в mode-line
(display-time-mode t)             ; показывать часы в mode-line
(size-indication-mode t)          ; размер файла в %-ах

(defun et/transparent-frame (bool)
  (if bool
      (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
    (set-frame-parameter (selected-frame) 'alpha '(100 . 100))))

(et/transparent-frame t)

(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

(use-package highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode)
         (prog-mode . hs-minor-mode))
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top))

(use-package org
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)  ; Заметки с отметкой времени
  (setq org-log-into-drawer t)
  (when (system-is-windows)
    (setq org-agenda-files '(;; Файлы GTD
			     "c:/Users/user/Dropbox/GTD/next_tasks.org"
			     "c:/Users/user/Dropbox/GTD/projects.org"
			     "c:/Users/user/Dropbox/GTD/journal.org"
			     "c:/Users/user/Dropbox/GTD/agenda.org"
			     "c:/Users/user/Dropbox/GTD/waiting.org"))
    (set 'inbox_file "c:/Users/user/Dropbox/GTD/inbox.org"))
  (when (system-is-linux)
    (setq org-agenda-files '(;; Файлы GTD
			     "~/Dropbox/GTD/next_tasks.org"
			     "~/Dropbox/GTD/projects.org"
			     "~/Dropbox/GTD/journal.org"
			     "~/Dropbox/GTD/agenda.org"
			     "~/Dropbox/GTD/waiting.org"))
    (set 'inbox_file "~/Dropbox/GTD/inbox.org"))


  (setq org-refile-targets
	'(("inbox.org" :maxlevel . 1)
	  ("projects.org" :maxlevel . 1)
	  ("journal.org" :maxlevel . 4)
	  ("someday.org" :maxlevel . 1)
	  ("next_tasks.org" :maxlevel . 1)
	  ("waiting.org" :maxlevel . 1)
	  ("agenda.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords '((sequence "TODO(t)"
				      "IN-PROGRESS(s)"
				      "PAUSE(p@/!)"
				      "NEXT(n@)"
				      "ACTIVE(a)"
				      "WAITING(w@/!)""|" "DONE(d!)" "CANCEL(c@)")))
  (setq org-tag-alist
	'((:startgroup)
					; Put mutually exclusive tags here
	  (:endgroup)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("meeting" .?m)
	  ("day" . ?d)
	  ("projects" . ?p)
	  ("next" . ?n)
	  ("waiting" . ?g)
	  ("sprint" .?s)))

  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-agenda-span 0)))
	    (tags-todo "+TODO=\"TODO\"-habits"
		       ((org-agenda-overriding-header "TODO")))
	    (todo "IN-PROGRESS"
		  ((org-agenda-overriding-header "IN-PROGRESS")))
	    (todo "WAITING"
		  ((org-agenda-overriding-header "WAITING")))
	    (todo "PAUSE"
		  ((org-agenda-overriding-header "PAUSE")))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next")))))

	  ("w" "Workflow Status"
	   ((tags-todo "projects"
		       ((org-agenda-overriding-header "Projects")
			(org-agenda-files org-agenda-files)))
	    (tags-todo "next"
		       ((org-agenda-overriding-header "Next")
			(org-agenda-files org-agenda-files)))
	    (tags-todo "waiting"
		       ((org-agenda-overriding-header "Waiting")
			(org-agenda-files org-agenda-files)))))
	  ("s" "Sprint"
	   ((tags-todo "sprint"
		       ((org-agenda-overriding-header "Sprint")
			(org-agenda-files org-agenda-files)))))

	  ("i" "Inbox"
	   ((todo "TODO"))((org-agenda-files (list inbox_file))))

	  ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"TODO\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(when (system-is-windows)
  (set 'gtd_inbox_file "c:/Users/user/Dropbox/GTD/inbox.org")
  (set 'gtd_journal_filel "c:/Users/user/Dropbox/GTD/journal.org")
  (set 'gtd_agenda_filel "c:/Users/user/Dropbox/GTD/agenda.org")
  (set 'gtd_notes_filel "c:/Users/user/Dropbox/GTD/notes.org")
  (set 'gtd_projects_filel "c:/Users/user/Dropbox/GTD/projects.org")
  (set 'gtd_someday_filel "c:/Users/user/Dropbox/GTD/someday.org")
  (set 'gtd_next_tasks_file "c:/Users/user/Dropbox/GTD/next_tasks.org")
  (set 'gtd_waiting_file "c:/Users/user/Dropbox/GTD/waiting.org"))
(when (system-is-linux)
  (set 'inbox_file "~/Dropbox/GTD/inbox.org")
  (set 'journal_filel "~/Dropbox/GTD/journal.org")
  (set 'gtd_agenda_filel "~/Dropbox/GTD/agenda.org")
  (set 'gtd_notes_filel "~/Dropbox/GTD/notes.org")
  (set 'gtd_projects_filel "~/Dropbox/GTD/projects.org")
  (set 'gtd_someday_filel "~/Dropbox/GTD/someday.org")
  (set 'gtd_next_tasks_file "~/Dropbox/GTD/next_tasks.org")
  (set 'gtd_waiting_file "~/Dropbox/GTD/waiting.org"))

(server-start)
(require 'org-protocol)

(setq org-capture-templates
      '(;; Захват задач в файл Inbox
	("i" "Inbox task")
	("ii" "Just a task (просто задача)" entry (file+olp gtd_inbox_file "Inbox")
	 "* TODO %?\n Entered on %U")
	("il" "Task with a link to a file (задача с ссылкой на файл))" entry (file+olp gtd_inbox_file "Inbox")
	 "* TODO %?\n Entered on %U \n %a")
	("im" "Meeting (собрание)" entry (file+olp gtd_agenda_filel "Future")
	 "* TODO %? :meeting: \n %^t %i")
	("is" "Schedule a task (запланировать задачу)" entry (file+olp gtd_inbox_file "Inbox")
	 "* TODO %? %^G \n SCHEDULED: %^t %i")
	("id" "Task with a deadline (задача с дедлайном)" entry (file+olp gtd_inbox_file "Inbox")
	 "* TODO %? %^G \n DEADLINE: %^t %i")
	;; Журнал дня
	("j" "Journal Entries")
	("jj" "Journal" entry (file+function gtd_journal_filel
					     (lambda ()
					       (org-datetree-find-date-create
						(org-date-to-gregorian (org-today)) t)
					       (re-search-forward "^\\*.+ Day" nil t)))
	 "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n")
	("jd" "Tasks for the day" entry
	 (file+olp+datetree gtd_journal_filel)
	 "\n* Day \n* Meeting :meeting: \n* %<%Y-%m-%d %p> - Tasks for the day")
	;; Заметки
	("n" "Note")
	("nn" "Note with link" entry  (file gtd_notes_filel)
	 "* Note (%a)\n Entered on/ %U\n %?")
	("nj" "Just a note" entry  (file gtd_notes_filel)
	  "* Note %?\n  Entered on/ %U\n")
	;; Захват задач из внешних источников (браузер)
	("c" "org-protocol-capture" entry (file+olp gtd_inbox_file "Inbox")
	 "* TODO [[%:link][%:description]]\n\n %i"
	 :immediate-finish t)
	;; Захват выделенного региона, используеться в функции et/org-capture-inbox
	("e" "capturing a selected region" entry (file+olp gtd_inbox_file "Inbox")
	 "* TODO %?\n %a\n %i"
	 :immediate-finish t)))

(defun et/org-capture-inbox ()
  "Записать выделенный регион в файл Inbox"
  (interactive)
  (org-capture nil "e"))

(when (system-is-windows)
  (set 'path_org_roam "c:/Users/user/Dropbox/Braindump/main"))
(when (system-is-linux)
  (set 'path_org_roam "~/Dropbox/Braindump/main"))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory path_org_roam)
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point)
	 :map org-roam-dailies-map
	 ("Y" . org-roam-dailies-capture-yesterday)
	 ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
	(file-name-nondirectory
	 (directory-file-name
	  (file-name-directory
	   (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error ""))))

(use-package org-roam-ui
  :after org-roam
  ;;normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;a hookable mode anymore, you're advised to pick something yourself
  ;;if you don't care about startup time, use
  ;;:hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(when (system-is-windows)
  (set 'center_org 130))
(when (system-is-linux)
  (set 'center_org 150))

(defun et/org-mode-visual-fill ()
    (setq visual-fill-column-width center_org
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package visual-fill-column
    :hook (org-mode . et/org-mode-visual-fill))

(use-package toc-org
  :after org
  :hook (org-mode . toc-org-enable))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("cs" . "src csharp"))

(defun et/open-inbox ()
  "Открыть файл Inbox"
  (interactive)
  (find-file gtd_inbox_file))

(defun et/open-agenda ()
  "Открыть файл Agenda"
  (interactive)
  (find-file gtd_agenda_filel))

(defun et/open-journal ()
  "Открыть файл Journal"
  (interactive)
  (find-file gtd_journal_filel))

(defun et/open-notes ()
  "Открыть файл Notes"
  (interactive)
  (find-file gtd_notes_filel))

(defun et/open-projects ()
  "Открыть файл Projects"
  (interactive)
  (find-file gtd_projects_filel))

(defun et/open-someday ()
  "Открыть файл Someday"
  (interactive)
  (find-file gtd_someday_filel))

(defun et/open-next-tasks ()
  "Открыть файл Someday"
  (interactive)
  (find-file gtd_next_tasks_file))

(defun et/open-waiting ()
  "Открыть файл Someday"
  (interactive)
  (find-file gtd_waiting_file))

(when (system-is-windows)
  (defvar et/path-expand "./myconfig.org"))
(when (system-is-linux)
  (defvar et/path-expand "~/.emacs.d/myconfig.org"))

(defun et/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name et/path-expand))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'et/org-babel-tangle-config)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (define-key dired-mode-map "h"
    'dired-single-up-directory)
  (define-key dired-mode-map "l"
    'dired-single-buffer))

(use-package dired-single)

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

(when (system-is-windows)
  (setq et/deft-dir-list '("c:/Users/user/Dropbox/Braindump/main"
                           "c:/Users/user/Dropbox/GTD/")))
(when (system-is-linux)
  (setq et/deft-dir-list '("~/Dropbox/Braindump/main"
                           "~/Dropbox/GTD/")))

(use-package deft
  :config (setq deft-directory "c:/Users/user/Dropbox/Braindump/main"
                deft-extensions '("md" "org"))
  (setq deft-use-filename-as-title t))

(defun et/pick-deft-dir ()
  "Select directories from a list"
  (interactive)
  (setq deft-directory
        (ido-completing-read "Select directory: " et/deft-dir-list))
  (deft-refresh))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
        :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
  :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-key] . helpful-key))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
    (setq which-key-idle-delay 1))

(use-package bs)

;; Добавим чтобы в буфере всегда был scratch
(setq bs-configurations
    '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))

(global-set-key (kbd "<f2>") 'bs-show)

(use-package dashboard
  :init
  (progn
    (setq dashboard-startup-banner "~/.emacs.d/images/ET_Light_Small.png")
    (setq dashboard-items '((recents  . 5)
                            (projects . 5)
                            (agenda . 5)))
    (setq dashboard-show-shortcuts nil)
    (setq dashboard-center-content t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-init-info t ))
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-banner-logo-title "Good Hack")
  ;; Кнопки навигации
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   (if (featurep 'all-the-icons)
       `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
           "Homepage" "Browse homepage"
           (lambda (&rest _) (browse-url "https://github.com/EgorTolbaev")))
          (,(all-the-icons-fileicon "elisp" :height 1.1 :v-adjust -0.1)
           "Configuration" "" (lambda (&rest _) (edit-configs)))))
     `((("" "Homepage" "Browse homepage"
         (lambda (&rest _) (browse-url "https://github.com/EgorTolbaev")))
        ("" "Configuration" "" (lambda (&rest _) (edit-configs)))))))
  ;; Настройки dashboard-agenda для показа с определенным тегом и статусом
  (dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (dashboard-match-agenda-entry "day|@work"
    dashboard-match-next-entry "TODO=\"TODO\"|TODO=\"IN-PROGRESS\"|TODO=\"PAUSE\""))

(use-package reverse-im
  :custom
    (reverse-im-input-methods '("russian-computer"))
  :config
    (reverse-im-mode t))

(use-package browse-url
  :ensure nil
  :custom
    (browse-url-browser-function 'browse-url-generic)
    (browse-url-generic-program "qutebrowser"))

(use-package engine-mode
  :defer 3
  :config
    (defengine duckduckgo
      "https://duckduckgo.com/?q=%s"
      :keybinding "d")

    (defengine github
      "https://github.com/search?ref=simplesearch&q=%s"
      :keybinding "g")

    (defengine google-images
      "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
      :keybinding "i")

    (defengine youtube
      "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
      :keybinding "y")

  (engine-mode t))

(use-package hydra
  :bind (("C-c b" . hydra-browser/body)
	 ("C-c t" . hydra-treemacs/body)
	 ("C-c s" . hydra-theme/body)
	 ("C-c o" . hydra-org/body)
	 ("C-c w" . hydra-windows/body)
	 ("C-c T" . hydra-text-scale/body)
	 ("C-c g" . hydra-file-gtd/body)
	 ))

(use-package major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust face)
    "Display an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-faicon (icon str &optional height v-adjust face)
    "Display an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon ':v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-fileicon (icon str &optional height v-adjust face)
    "Display an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-octicon (icon str &optional height v-adjust face)
    "Display an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str)))

(pretty-hydra-define hydra-browser
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "chrome" "Browser" 1 -0.05))
  (""
   (("d" engine/search-duckduckgo "Duckduckgo")
    ("i" engine/search-google-images "Google images")
    ("y" engine/search-youtubes "Youtube")
    ("g" engine/search-github "GitHub"))))

(pretty-hydra-define hydra-treemacs
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "file-text" "Treemacs" 1 -0.05))
  (""
   (("t" treemacs "Treemacs")
    ("s" lsp-treemacs-symbols "Treemacs Symbols"))))

(pretty-hydra-define hydra-theme
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "codepen" "Theme" 1 -0.05))
  ("All Theme"
   (("a" counsel-load-theme "View all themes"))
   "Used themes"
   (("d" set-night-theme "Night theme")
    ("l" set-light-theme "Light theme"))
   "Frame"
   (("p" (et/transparent-frame t) "Transparent frame")
    ("n" (et/transparent-frame nil) "Not transparent frame"))))

(pretty-hydra-define hydra-org
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "codepen" "Org" 1 -0.05))
  (""
   (("g" org-insert-link-global "Insert link")
    ("l" org-store-link "Store link")
    ("c" org-capture "Create capture")
    ("a" org-agenda "Open agenda"))
   "Clock"
   (("j" org-clock-goto "Org clock goto")
    ("d" org-clock-in-last "Org clock in last")
    ("i" org-clock-in "Org clock in")
    ("o" org-clock-out "Org clock uot")
    ("t" org-clock-report "Org clock report"))))

(pretty-hydra-define hydra-windows
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "windows" "Windows" 1 -0.05))
  (""
   (("g" golden-ratio-mode "Golden ratio")
    ("b" balance-windows   "Balance windows"))
   ""
   (("s" shrink-window               "Shrink window")
    ("[" shrink-window-horizontally  "Shrink window horizontally")
    ("]" enlarge-window-horizontally "Enlarge window horizontally"))))

(pretty-hydra-define hydra-text-scale
  (:hint nil :forein-keys warn :quit-key "q" :timeout 4 :title (with-faicon "codepen" "Text" 1 -0.05))
  (""
   (("j" text-scale-increase "in")
    ("k" text-scale-decrease "out"))))

(pretty-hydra-define hydra-file-gtd
  (:hint nil :forein-keys warn :quit-key "q" :timeout 4 :title (with-faicon "codepen" "GTD" 1 -0.05))
  ("File GTD"
   (("i" (et/open-inbox) "Open Inbox")
    ("a" (et/open-agenda) "Open Agenda")
    ("j" (et/open-journal) "Open Journal")
    ("n" (et/open-notes) "Open Note"))
   ""
    (("p" (et/open-projects) "Open Project")
    ("s" (et/open-someday) "Open Someday")
    ("t" (et/open-next-tasks) "Open Next task")
    ("w" (et/open-waiting) "Open Waiting"))))

(defun et/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . et/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "C-x t s") 'lsp-treemacs-symbols)

(use-package treemacs-icons-dired
  :config (treemacs-icons-dired-mode))

(use-package lsp-ivy)

(use-package dap-mode)

;(use-package typescript-mode
;  :mode "\\.ts\\'"
;  :hook (typescript-mode . lsp-deferred)
;  :config
;  (setq typescript-indent-level 2)
;  (require 'dap-node)
;  (dap-node-setup))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :hook (js2-mode . lsp-deferred)
  :config
  (setq js-indent-level 2))

(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package pyenv-mode
  ;; Integrate pyenv with Python-mode
  :hook (python-mode . pyenv-mode)
  :init
  (let ((pyenv-path (expand-file-name "~/.pyenv/bin")))
    (setenv "PATH" (concat pyenv-path ":" (getenv "PATH")))
    (add-to-list 'exec-path pyenv-path))
  :config
  (pyenv-mode))

(use-package csproj-mode)
(use-package dotnet)

(use-package csharp-mode
  :mode "\\.cs\\'"
  :hook ((csharp-mode . lsp-deferred)
	 (csharp-mode . dotnet-mode))
  :config
  (require 'dap-netcore))

(defun et/dotnet-run ()
  (interactive)
  (setq dotnet-run-last-proj-dir nil)
  (dotnet-run dotnet-run-last-proj-dir))

(use-package company
  ;:after lsp-mode
  ;:hook (lsp-mode . company-mode)
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package web-mode
  :mode (("\\.css$"  . web-mode)
         ("\\.html$" . web-mode)))

(use-package json-mode
  :mode "\\.json\\'")

(use-package yaml-mode
  :mode (("\\.yml$"  . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

(use-package makefile-gmake-mode
  :ensure nil
  :mode  ("Makefile.*" . makefile-gmake-mode))

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode)

(use-package projectile
  :config
    (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
    (projectile-mode +1))

(use-package magit
  :bind (("C-x g" . #'magit-status)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun et/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . et/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'multiline2))
