;;;pomodoro.el --- Let it snow in Emacs!         -*- lexical-binding: t; -*-

;; Звку для таймера
(setq org-clock-sound "~/.emacs.d/local/pomodoro/resources/bell.wav")

(defun et/pomedoro (time)
  "Запустить таймер pomedor"
  (interactive)
  (org-timer-set-timer time))

(defun et/pomedoro25 ()
  "Запустить таймер pomedor 25 мин."
  (interactive)
  (et/pomedoro "00:25:00"))

(provide 'pomodoro)
