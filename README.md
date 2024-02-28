# Step by step code and explanation

Supplementary material for An "Incremental Approach to Compiler Construction"

## Install common lisp - sbcl

Because `mit-scheme` doesn't build on Macos M1.

On Macos,

```
brew install sbcl
```

## Install package manager and other tools.

On Debians,

```
apt-get install sbcl
```

Restating instructions from [Alive LSP's Readme](https://github.com/nobody-famous/alive-lsp?tab=readme-ov-file#install-a-librarypackage-manager),

```
curl -O https://beta.quicklisp.org/quicklisp.lisp
curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
gpg --verify quicklisp.lisp.asc quicklisp.lisp
sbcl --non-interactive --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)'
```

`(ql:add-to-init-file)` is optional but recommended for newcomers.

> [!NOTE]
> The files, quicklisp.lisp and quicklisp.lisp.asc are already checked into the repository
> for convenience. I'm leaving these instruction as reference to start any new common lisp
> project. Cheers!

Run the LSP with the following in vscode.

```
{
    "alive.lsp.startCommand": [
        "sbcl",
        "--eval",
        "(require :asdf)",
        "--eval",
        "(asdf:load-system :alive-lsp)",
        "--eval",
        "(alive/server:start)"
    ]
}
```

> [!NOTE]
> You test the LSP server on the terminal with,
> `sbcl --non-interactive --eval '(ql:quickload "alive-lsp")' --eval '(alive/server::start :port 8006)'`

## Build the compiler

```
sh ./dev.sh
```
