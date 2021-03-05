;; Определение ОС
(defun system-is-linux()
  (string-equal system-type "gnu/linux"))
(defun system-is-windows()
  (string-equal system-type "windows-nt"))

;; Подсветка lisp выражений
(setq show-paren-style 'expression)
(show-paren-mode 2)
(setq auto-mode-alist
      (append
       '(
         ( "\\.el$". lisp-mode))))
(global-font-lock-mode 1)

;; Подключение репозиториев пакетов
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Если use-package не установлен, его нужно скачать и установить
(unless (package-installed-p 'use-package)
  (message "Emacs will install use-package.el")
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Переносим переменные, созданные Custom в отдельный файл
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Кодировка
(set-language-environment 'UTF-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read    'utf-8)
(setq file-name-coding-system           'utf-8)
(set-keyboard-coding-system        'utf-8-unix)
(set-terminal-coding-system             'utf-8)
(prefer-coding-system                   'utf-8)

;; Замена yes-or-on на y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Размер окна
(when (window-system)
  (set-frame-size (selected-frame) 100 40))

;; Работа с проектами
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))

;; Тема
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-Iosvkem))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Красивые иконки в дашборде, treemacs и т.д
(use-package all-the-icons
  :ensure    t)

;; Пока так зато есть быстрое переключение тем
(defun set-light-theme()
  (interactive)
  (load-theme 'doom-opera-light))
  (global-set-key (kbd "<f8>") 'set-light-theme)

(defun set-night-theme()
  (interactive)
  (load-theme 'doom-Iosvkem))
  (global-set-key (kbd "<f9>") 'set-night-theme)

;; Что бы работало первый раз нужно добавить шрифты,
;; на винде нужно еще выбрать путь куда поставить и установить их в ручную
;; Оставлю здесь чтобы не забыть:)
;; (all-the-icons-install-fonts)

;; Стартовый экран
(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-startup-banner 'logo)
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

;; Дерево каталогов
(use-package treemacs
  :ensure    t
  :bind      ("M-n M-n" . #'treemacs))

;; Отключение меню
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode   -1) ; отключаем полосу прокрутки
(blink-cursor-mode -1) ; курсор не мигает
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал

;; Отключить  экран приветствия
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t) ; экран приветствия можно вызвать комбинацией C-h C-a

;; Нумерация строк слева
(require 'linum) ; вызвать Linum
(setq line-number-mode   nil) ; показать номер строки в mode-line
(global-linum-mode  t)        ; показывать номера строк во всех буферах
(setq column-number-mode nil) ; показать номер столбца в mode-line
(setq linum-format " %d")     ; задаем формат нумерации строк
;; Установка фиксированной высоты чтобы нумерация не меняла размер
;; например в режиме org-mode
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))

;; Отключить  сохранений
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

;; Управление git-репозиторием
(use-package magit
  :ensure    t
  :bind      (("C-x g" . #'magit-status)))

;; Автозаполнение при поиске в каталоге
(use-package ido
  :ensure t
  :config
  (ido-mode t)
  (ido-everywhere t)
  (icomplete-mode t)
  (setq ido-virtual-buffers t)
  (setq ido-enable-flex-matching t))

;; Настройка org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done 'time)

;; Показ буфера
(use-package bs
  :ensure t)
;; Добавим чтобы в буфере всегда был scratch
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))
(global-set-key (kbd "<f2>") 'bs-show)

(use-package sr-speedbar
  :ensure t
  :config
  (setq sr-speedbar-right-side nil))
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)

;; Удалять пробелы в конце строки при сохранении
(add-hook 'before-save-hook '(lambda ()
			       (delete-trailing-whitespace)))

(setq display-time-24hr-format t) ; 24-часовой временной формат в mode-line
(display-time-mode             t) ; показывать часы в mode-line
(size-indication-mode          t) ; размер файла в %-ах

(setq word-wrap          t) ; переносить по словам
(global-visual-line-mode t)

;; При вводе парного элемента закрывать его и ставить курсор между ними
(electric-pair-mode t)
(show-paren-mode 1)

;; Работа emacs в русской раскладке
(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

;; Автокомплит
(use-package company
  :ensure    t)

;; Чтобы использовать company-mode во всех буферах
(add-hook 'after-init-hook 'global-company-mode)

;; Иконки в буфере автокомплита
(use-package company-box
  :ensure    t
  :hook      (company-mode . company-box-mode))
