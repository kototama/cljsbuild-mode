# Cljs-Build


An Emacs minor mode for the ClojureScript 'lein cljsbuild' command
that will automatically watch the compilation buffer, pops it when the
compilation failed and (optionally) hides it when the compilation
succeed.

## How to use it?

1. Install the mode with M-x package-install RET cljsbuild-mode
2. Add (require 'cljsbuild-mode) in your ~/.emacs.d/init.el file
3. Start a terminal with M-x term or M-x multi-term
4. Run 'lein cljsbuild auto' in it
5. Start cljsbuild-mode in the terminal buffer with M-x cljsbuild-mode
6. Enjoy!

