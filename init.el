;; Определение ОС
(defun system-is-linux()
  (string-equal system-type "gnu/linux"))
(defun system-is-windows()
  (string-equal system-type "windows-nt"))

;; подсветка lisp выражений
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

;;замена yes-or-on на y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; ######################## Настройка интерфейса #######################

(setq frame-title-format "Egor Tolbaev")

;; Тема
(use-package dracula-theme
  :ensure    t
  :config
(if
   (and
     (>= (string-to-number (format-time-string "%H")) 10)
     (< (string-to-number(format-time-string "%H")) 20))
    (load-theme 'dracula t)
  (load-theme 'tango t)))

;; Красивые иконки в дашборде, treemacs и т.д
(use-package all-the-icons
  :ensure    t)

;; Стартовый экран
(use-package dashboard
  :ensure    t
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-center-content t
        dashboard-set-init-info nil
        dashboard-set-heading-icons t
        dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))

;; Дерево каталогов
(use-package treemacs
  :ensure    t
  :bind      ("M-n M-n" . #'treemacs))

;; Размер окна
(when (window-system)
  (set-frame-size (selected-frame) 100 40))

;;Отключение меню
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode   -1) ;; отключаем полосу прокрутки
(blink-cursor-mode -1) ;; курсор не мигает
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал

;; Отключить  экран приветствия
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t) ;; экран приветствия можно вызвать комбинацией C-h C-a

;; Нумерация строк слева
  (global-linum-mode 1)

;; Измененый модлайн
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

;; #####################################################################

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

(setq display-time-24hr-format t) ;; 24-часовой временной формат в mode-line
(display-time-mode             t) ;; показывать часы в mode-line
(size-indication-mode          t) ;; размер файла в %-ах

(setq word-wrap          t) ;; переносить по словам
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
;; Чтобы использовать company-modeво всех буферах
(add-hook 'after-init-hook 'global-company-mode)
;; Иконки в буфере автокомплита
(use-package company-box
  :ensure    t
  :hook      (company-mode . company-box-mode))
