;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode . ((dante-repl-command-line . ("nix-shell" "--attr"
					      "generator"
					      (expand-file-name
					       (file-name-directory
						(directory-file-name dante-project-root)) ) "--run" "cabal repl --builddir=dist/dante")))))
