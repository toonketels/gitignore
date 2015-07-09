gitignore
---------

Adds gitignore rules to project `.gitignore` file or creates it if it does not yet exist.

Rules are defined in templates within the `~/.gitignore/` directory. Templates are
regular `.gitignore` files. They are named `<name>.gitignore`. Add the rules in the `<name>`
template to your project's `.gitignore``by doing:

    cd my/project/root
    gitignore <name>


Multiple templates can be added at once like

    gitignore haskell vim

which adds the rules in the `haskell` and `vim` templates (`haskell.gitignore` and
`vim.gitignore` in`~/.gitignore/` directory) to your project's `.gitignore`.


Installation
============

To compile you'll need `haskell` and `cabal` on your system. Move into this project and do:

    cabal install
    cabal build

The binary will be located in `dist/build/gitignore/gitignore`. Add it to your path.
