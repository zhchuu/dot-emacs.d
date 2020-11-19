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
    | lisp
        | color-theme.el            # Themes collection
        | init-global-settings.el   # Some basic global settings
        | init-theme.el             # Initialize theme
        | init-elpa.el              # Initialize ELPA
        | init-xxx.el               # init-[package_name].el
        | ...                       # More
        | langs
            | my-python-mode.el     # Configuration of python mode
            | my-xxx-mode.el        # my-[language_name]-mode.el
            | ...                   # More
        | themes
            | some-theme.el         # Themes
            | ...                   # More
```
