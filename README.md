<p align="center"><img src="assets/emacs-logo.svg" width=150 height=150/></p>
<p align="center"><a href="https://www.gnu.org/software/emacs/"><b>GNU Emacs</b></a></p>
<p align="center">
	<a href="https://www.gnu.org/software/emacs/"><img src="https://img.shields.io/badge/GNU%20Emacs-27.1-b48ead.svg?style=flat-square"/></a>
	<a href="https://orgmode.org/"><img src="https://img.shields.io/badge/org--mode-9.5-489a9f.svg?style=flat-square"/></a>
	<a href="https://github.com/jwiegley/use-package"><img src="https://img.shields.io/badge/use--package-2.4.1-88c0d0.svg?style=flat-square"/></a>
</p>
<p align="center">This repository contains all my GNU Emacs configuration.</p>

---

<p align="center"><img src="assets/screenshots/emacs-preview.png"/></p>

<details> 
  <summary>Screenshots</summary>
  Dashboard
  <p align="center"><img src="assets/screenshots/dashboard.png"/></p>
  Org-mode
  <p align="center"><img src="assets/screenshots/org-mode.png"/></p>
  Dired
  <p align="center"><img src="assets/screenshots/dired.png"/></p>
  Treemacs
  <p align="center"><img src="assets/screenshots/treemacs.png"/></p>
</details>

<br/>

<blockquote>
    Mode Line: <a href="https://github.com/seagle0128/doom-modeline">DOOM Modeline</a> <br>
	Theme:<br>
    &nbsp;&nbsp;&nbsp;&nbsp;Dark theme: <a href="https://github.com/hlissner/emacs-doom-themes/blob/screenshots/doom-Iosvkem.png">doom-Iosvkem</a><br>
    &nbsp;&nbsp;&nbsp;&nbsp;Light theme: <a href="https://github.com/hlissner/emacs-doom-themes/blob/screenshots/doom-opera-light.png">doom-opera-light</a>
    <br>
    Font: <a href="https://github.com/adobe-fonts/source-code-pro">Source Code Pro</a>
</blockquote>

<br/>

List of files:

- [`myconfig.org`](https://github.com/EgorTolbaev/.emacs.d/blob/master/myconfig.org):
  main configuration file.

- [`init.el`](https://github.com/EgorTolbaev/.emacs.d/blob/master/init.el):
  load the generated configuration file.

## Usage Instructions

---

<br/>

### Linux и macOS

<br/>

On Linux, Emacs can be found in every major Linux distribution’s package manager. On macOS you can install Emacs using Homebrew via brew install emacs.

You can easily get started with this configuration on Linux and macOS by following these steps:

1. Clone this repository to a folder on your system:

```
git clone https://github.com/EgorTolbaev/.emacs.d
```

2. Back up any existing Emacs configuration you may already have in your home directory in the `.emacs.d` folder
3. Start Emacs!

<br/>

### Windows

<br/>

You can download the latest Emacs version for Windows by heading to the [GNU FTP site](https://ftp.gnu.org/gnu/emacs/windows/). You can also [install Emacs via Chocolatey](https://community.chocolatey.org/packages/Emacs) if you prefer a more automated approach.

The location where the Emacs configuration folder lives may vary based on your version of Windows. If you have trouble getting this to work, check the [official FAQ](https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Location-of-init-file.html#Location-of-init-file) on the topic.

For most modern Windows versions (Windows 7+), these steps should work:

1. Clone this repository to a folder on your system:

```
git clone https://github.com/EgorTolbaev/.emacs.d
```

2. Back up any existing Emacs configuration you may already have in your home directory (likely C:\Users\yourusername\ or C:\Users\yourusername\AppData\Roaming\) in the `.emacs.d` folder
3. Start Emacs!

## Additional settings

<br/>

### Installing fonts

---

This configuration uses the [Source Code Pro](https://github.com/adobe-fonts/source-code-pro) font by [@adobe-fonts](https://github.com/adobe-fonts) and installation instructions can be found on [this wiki page](https://github.com/adobe-fonts/source-code-pro/releases/tag/2.038R-ro%2F1.058R-it%2F1.018R-VAR).

In this configuration, one more font is used that will need to be installed for Windows, in linux it is probably there.

[Cantarell](https://github.com/GNOME/cantarell-fonts) download it with [google fonts](https://fonts.google.com/specimen/Cantarell).

However, since this is meant to be your configuration, feel free to choose your own font!

<br/>

### Markdown

---

For markdown to work, you need to install its processor. In this configuration, I use [pandoc](https://pandoc.org/), instructions on how to install it can be found [here](https://pandoc.org/installing.html).

You can also use whichever markdown processor you like, just install it and make changes to the [configuration](https://github.com/EgorTolbaev/.emacs.d/blob/master/myconfig.org#markdown).

<br/>

### Qutebrowser

---

In place with Emacs, I use [Qutebrowser](https://qutebrowser.org/), use it in place with browse-url and engine-mode, to install it read the [instructions](https://qutebrowser.org/doc/install.html).

If you are interested in my Qutebrowser configuration, you can see it [here](https://github.com/EgorTolbaev/dotfiles/tree/master/qutebrowser).

If for some reason you do not want to use this, make changes to the [configuration](https://github.com/EgorTolbaev/.emacs.d/blob/master/myconfig.org#browser).

---

**Feel free to file an issue**
