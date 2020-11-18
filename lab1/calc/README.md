# Haskell Template

This is the Haskell template for the ID2202 course. Like the other
templates, building happens with `make`, running with `./hello`. The
project is built using
[Stack](https://docs.haskellstack.org/en/stable/README/), which you
must thus install, but there should be no other dependencies, e.g.,
you do not need to install GHC or the Haskell platform.

## Renaming the executable

To rename the produced executable (which some assignments may require
you to do), please edit the last argument to the `cp` command in
`Makefile` (also remember to ignore the new file in `.gitignore`):

```make
# Before
all:
	stack build --allow-different-user
	cp $(shell stack path --local-install-root --allow-different-user)/bin/hello hello

# After
all:
	stack build --allow-different-user
	cp $(shell stack path --local-install-root --allow-different-user)/bin/hello new-fancy-name
```

## Dependencies and Configuration

You add external dependencies in `exe.cabal` under
`build-depends`. This field is already pre-populated with a few
convenient packages, feel free to use them or ignore them.

On version numbers: the project is built with Stack which works by
taking a very large subset of [Hackage](https://hackage.haskell.org/)
with mutually compatible versions, then freezing those in package
sets. Since we are not publishing a library, we're just making an
executable, it's thus sufficient to merely mention which packages we
want, the version is already frozen by Stack.

The package set used by Stack can be found in `stack.yaml` under
`resolver`. We strongly suggest you do not change this, since it means
that the grader won't have your packages in its cache, meaning the
build will be significantly slower. To look at which packages are
included in this package set, or look at documentation for a
particular included version, see
[Stackage](https://www.stackage.org/).

## Splitting a Project Into Multiple Modules

Stack/Cabal does not auto-discover modules you have added to your
project, you must manually add them to `other-modules` in
`exe.cabal`. While this is annoying, there are apparently good reasons
for it, feel free to do some searching if you're interested (or
annoyed).
