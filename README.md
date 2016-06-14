# My Emacs configuration

![screenshot](https://raw.githubusercontent.com/AnimatedRNG/emacs-config/4b20aae9c534cd1f78a36c6576f6e2d4f2090ba4/img/screenshot_1.png)

I'm pretty new to Emacs; so I'm sure there are more elegant ways
to handle the config file.

This configuration has the following settings/features/tweaks:

#### Tweaks

* No backup files
* No start screen
* No tool bar
* Yes-or-no is y-or-n
* Smooth scrolling (with and without mouse)

#### Packages

* Major package archives enabled (GNU, Marmalade, MELPA)
* use-package enabled

#### Syntax-checking

* Flycheck enabled
* CPPCM is kind of working
* TODO: Autocomplete

#### Keybindings

* "M-s" is now `save`
* "C-u" is now `undo`
* "M-n" moves down five lines
* "M-p" moves up five lines
* Many common "C-x \*" keybindings are now "Super-\*"
* "M-w" and "C-w" without any arguments will copy/kill the current line
* Replacing text works now

#### Formatting

* "Super-a" formats code with astyle; tries to retain cursor position
* Indents are 4 spaces
* Redo is linear now

#### Aesthetics

* Column number shown as well
* Lines with over 80 characters will be highlighted
* A couple themes installed -- automatically fetches them from the
  elpa folder
* Font size is much easier on the eyes (height of 150)
* Lines are truncated rather than wrapped
* Cursor is a bar
* Sublimity plugin


## Install

**Do not clone this respository to your home directory!**

Clone the respository to a different folder and symlink the `.emacs` file
and the `.emacs.d` directory to your home directory.