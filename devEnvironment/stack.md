stack
=====

## build tool for Haskell

* is not a package manager, it is a build tool.
    It does not manage a set of “installed” packages;
    it simply builds targets and their dependencies.
* installs a compiler (GHC) in a system-independent location
    to avoid collisions and make builds more reproducible
* installs packages needed for your project
* builds the project with all its dependencies (uses cabal under the hood)
* benchmarks the project

but in general the goal is to create easily managed dev environment
and high portability

### base commands

`stack new projectName`

* this will create the base setup of the project,
    including base directories and files, mainly:
    ```
        /app
        /src
        /test
        package.yaml
        stack.yaml
    ```

`stack build [executables | test | library]`
* will build the target, if no target is specified, stack
    will build all the targets in `package.yaml`

stack has a bunch of commands, but a lot of them are actually
just an alias to `stack build`

#### commands that are discouraged

- never use `stack install`! (== `stack install --copy-bins`)
    builds target and puts the executables in `~/.local/bin`
    Only use this command if installing a binary that has
    nothing to do with Haskell development. Do not do this:
    `stack install ghc-mod` or `stack install intero either`
    This will install this into the same directory and
    the whole point of having stack packaging everything nicely
    in an isolated environment.
- do not build projects while in development with `stack build`
    this includes compiler optimizations, thus slows compile time

#### command that are advised

+ use `stack build --fast` in order to omit optimizations
+ to run tests you can run `stack test --fast`, but again
    it is just an alias to `stack build --test --fast`
+ in order to build documentation for dependencies alongside the code use
    flag `--haddock`, but in order to only run once `--haddock-deps`
+ in order to build on file save you can use `--file-watch` flag,
    which will build and test the project in the background

### Accessing local documentation

stack installs documentation for all the dependencies locally with
all the versions matching (isn't it nice??)

#### ways to access
* `stack haddock --open lens` will open the documentation
    in your default browser (served locally!)
* of course searchable docs are more convenient, thus introducing
    a local `Hoogle` !
    `stack hoogle -- generate --local` to create the local database
    with all the documentation for the installed packages
    `stack hoogle -- server --local --port=8080`
    will serve the db on the localhost
* do not forget to generate the db when updating the packages

### Configuring project

#### `stack.yaml`
* file is a specification of precisely **which
    version of each package** should be used and
    where it should be fetched from
* controls which packages are built and what **version to
    pin the dependencies to**

#### `package.yaml`
* `projectName.cabal` will be generated using this file
* one can think of this file as a specification for **how
    your project is built** and **what packages** it depends on
* this is a file, which specifies build targets, their
    dependencies, and which **GHC options to apply**, among other things.
* sort of similar to `Makefile`, defines targets,

### Setting up editor

(Notes for **Atom** editor)

* atom needs ghc-mod in order for the integrations to work correctly
* you **should not run** `stack install ghc-mod`, because you would want
    the integration be consistent with the project's ghc version.
* "As mentioned above, stack install is not what you want.
    Tools like **ghc-mod, hlint, hoogle, weeder, and intero**
    work best when installed as part of the sandbox, not globally"
*








[source article](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)
