;;; init.el --- GNU Emacs Configuration

;; Author: Egor Tolbaev <egor05.09.97@gmail.com>
;; Homepage: https://github.com/EgorTolbaev/.emacs.d

;;; Code:

;; Ускоряем запуск за счет уменьшения частоты мусора
(setq gc-cons-threshold (* 50 1000 1000))

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

(setq auto-mode-alist
          (append '(("\\.org\\'" . org-mode)
                    ("\\.py\\'" . python-mode))
                  auto-mode-alist))

(org-babel-load-file (expand-file-name "~/.emacs.d/myconfig.org"))

(setq gc-cons-threshold (* 10 1000 1000))

;;; init.el ends here
