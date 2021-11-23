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
                calendar-mode-hook))
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
     '(
       ("\\.el$"  . lisp-mode)
       ("\\.org$" . org-mode))))

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

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

(defun et/org-mode-setup ()
(org-indent-mode)
;;(variable-pitch-mode 1)
(visual-line-mode 1))

(use-package org
  :hook (org-mode . et/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)  ; Заметки с отметкой времени
  (setq org-log-into-drawer t)

  (when (system-is-windows)
    (setq org-agenda-files '("c:/Users/user/Dropbox/OrgFiles/tasks/job.org"
                             "c:/Users/user/Dropbox/OrgFiles/tasks/house.org"
                             "c:/Users/user/Dropbox/OrgFiles/tasks/study.org"
                             "c:/Users/user/Dropbox/OrgFiles/tasks/meet.org"
                             "c:/Users/user/Dropbox/OrgFiles/holidays/Birthdays.org")))
  (when (system-is-linux)
    (setq org-agenda-files '("~/Dropbox/OrgFiles/tasks/job.org"
                             "~/Dropbox/OrgFiles/tasks/house.org"
                             "~/Dropbox/OrgFiles/tasks/study.org"
                             "~/Dropbox/OrgFiles/holidays/Birthdays.org")))

  (setq org-todo-keywords '((sequence "TODO(t)"
                                      "IN-PROGRESS(s)"
                                      "PAUSE(p@/!)"
                                      "NEXT(n)"
                                      "ACTIVE(a)"
                                      "WAITING(w@/!)""|" "DONE(d!)" "CANCEL(c@)")))

  (setq org-tag-alist
   '((:startgroup)
      (:endgroup)
      ("@home" . ?H)
      ("@work" . ?W)
      ("agenda" . ?a)
      ("meeting" .?m)
      ("note" . ?n)
      ("idea" . ?i)))

  (setq org-agenda-custom-commands
    '(("d" "Meetings today" tags-todo "+SCHEDULED>=\"<today>\"+SCHEDULED<\"<tomorrow>\"+meeting/ACTIVE"))))


(global-set-key (kbd "C-c l") 'org-store-link) ; Создать ссылку
(global-set-key (kbd "C-c c") 'org-capture)    ; Создать заметку
(global-set-key (kbd "C-c a") 'org-agenda)     ; Открыть agenda

(when (system-is-windows)
  (set 'path_note    "c:/Users/user/Dropbox/OrgFiles/notes.org")
  (set 'path_journal "c:/Users/user/Dropbox/OrgFiles/Journal.org")
  (set 'path_meeting "c:/Users/user/Dropbox/OrgFiles/tasks/meet.org")
  (set 'path_task    "c:/Users/user/Dropbox/OrgFiles/tasks/job.org"))
(when (system-is-linux)
  (set 'path_note    "~/Dropbox/OrgFiles/notes.org")
  (set 'path_journal "~/Dropbox/OrgFiles/Journal.org")
  (set 'path_meeting "~/Dropbox/OrgFiles/tasks/meet.org")
  (set 'path_task    "~/Dropbox/OrgFiles/tasks/job.org"))

(setq org-capture-templates
      '(("n" "Notes" entry (file+headline path_note "Notes")
         "* TODO %? %^g \nCreated %U\n  %i\n")

        ("m"  "Meeting work")
        ("mn" "New meeting work" entry (file+olp path_meeting "New meetings")
         "* ACTIVE %? :meeting: \nSCHEDULED: %^t  %i" :empty-lines 1)

        ("w"  "Work")
        ("wn" "New task" entry (file+olp path_task "Tasks")
         "* TODO %?\nSCHEDULED:  %^t \nDEADLINE: %^t  %i" :empty-lines 1)

        ("j" "Journal Entries")
        ("jj" "Journal" entry
         (file+olp+datetree path_journal)
         "\n* %<%I:%M %p> - %? :journal:\n\nNote:\n\n"
         :clock-in :clock-resume
         :empty-lines 1)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun et/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
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
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single)

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

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
                             (projects . 5)))
     (setq dashboard-show-shortcuts nil)
     (setq dashboard-center-content t)
     (setq dashboard-set-file-icons t)
     (setq dashboard-set-heading-icons t)
     (setq dashboard-set-init-info t ))
  :config
   (dashboard-setup-startup-hook))

;; Кнопки навигации
(setq dashboard-set-navigator t)

(setq dashboard-navigator-buttons
    `(
      ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
       "Homepage"
       "Browse homepage"
       (lambda (&rest _) (browse-url "https://github.com/EgorTolbaev"))))))

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

(use-package python-mode
  :ensure t
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

(use-package csharp-mode
  :ensure t
  :mode "\\.cs\\'"
  :hook (csharp-mode . lsp-deferred)
  :config
  (require 'dap-netcore))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
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

(use-package projectile
  :config
    (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
    (projectile-mode +1))

(use-package magit
  :bind   (("C-x g" . #'magit-status)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
