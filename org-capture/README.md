# Регистрация org-protocl

*Все файлы указанные ниже предназначены для моей системы, для того что бы это работало у Вас могут потребоваться изменения*

В данной папке лежат скрипты для регистрации org-protocol в системе.

## Windows

Для регистрации org-protocol в Windows предназначен файл **org-capture.reg**, он содержит следующий код:

```bash
REGEDIT4

[HKEY_CLASSES_ROOT\org-protocol]
@="URL:Org Protocol"
"URL Protocol"=""
[HKEY_CLASSES_ROOT\org-protocol\shell]
[HKEY_CLASSES_ROOT\org-protocol\shell\open]
[HKEY_CLASSES_ROOT\org-protocol\shell\open\command]
@="\"C:\\emacs\\bin\\emacsclientw.exe\" \"%1\""
```

***Обратите внимание что строка*** @"\\"C:\\\emacs\\\bin\\\emacsclientw.exe\\" \\"%1\\"" ***может отличаться, в зависимости куда был установлен GNU Emacs***

## GNU/Linux

Для регистрации org-protocol в GNU/Linux предназначен файл **org-protocol.desktop**, он содержит следующий код:

```bash
[Desktop Entry]
Name=org-protocol
Comment=Intercept calls from emacsclient to trigger custom actions
Categories=Other;
Keywords=org-protocol;
Icon=emacs
Type=Application
Exec=emacsclient -- %u
Terminal=false
StartupWMClass=Emacs
MimeType=x-scheme-handler/org-protocol;
```

Подробней можно прочитать [тут](https://orgmode.org/worg/org-contrib/org-protocol.html).
