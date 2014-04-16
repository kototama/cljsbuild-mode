# Cljs-Build


An Emacs minor mode for the ClojureScript 'lein cljsbuild' command
that will automatically watch the compilation buffer, pops it when the
compilation failed and (optionally) hides it when the compilation
succeed.

## Installation

Packages are available in the Marmalade and
[MELPA](http://melpa.milkbox.net/) repositories.  Install the mode
with <kbd>M-x package-install RET cljsbuild-mode</kbd>.

## Usage

1. <kbd>M-x cljsbuild-start</kbd>
2. <kbd>M-x cljsbuild-stop</kbd>
3. Enjoy!

Alternatively, if you prefer to work from a terminal:

1. Start a terminal with <kbd>M-x term</kbd> or <kbd>M-x multi-term</kbd>
2. Run `lein cljsbuild auto` in it
3. Start `cljsbuild-mode` in the terminal buffer with <kbd>M-x cljsbuild-mode</kbd>

