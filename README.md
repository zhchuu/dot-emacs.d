# dot-emacs.d
My emacs configuration.

## Introduction
The configuration should run on Emacs 25.3.1 or greater.

## Structure
```
~/.emacs.d
    | README.md
    | LICENSE
    | init.el
    | inits
        | init-global-settings.el   # Some basic global settings
        | init-theme.el             # Initialize theme
        | init-elpa.el              # Initialize ELPA
        | init-xxx.el               # init-[package_name].el
    | lisp
        | color-theme.el            # Themes collection
        | ...                       # More
        | themes
            | some-theme.el         # Themes
            | ...                   # More
```
