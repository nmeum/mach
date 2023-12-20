## README

A WiP implementation of `make(1)`, aims to be compatible with [P1003.1™-202x/D3][posix draft] at some point.
The name refers to the transitive verb for [*machen*][wiktionary machen], the German translation of the word *make*.

### Motivation

The goal is to write a clean, easily extendible implementation of POSIX make in order to enable experiments with fancy new make features.
For example, [bazel][bazel web]-like [sandboxing][bazel sandbox] (executing each target in a Linux namespace) to find impure dependencies in existing Makefiles, or debugging features such as the ones provided by the [remake project][remake github].

### Status

Presently, the implementation is mainly a proof of concept.
Many parts of the POSIX standard are not implemented and some parts are implemented in a not fully conforming way.
However, some software which more simple Makefile configurations can already be compiled using mach, e.g. [pdpmake][pdpmake makefile].
Refer to the equivalence tests against a known-good make implementation in `tests/golden` for more information on what is known to work right now.
Further information is also available in the `TODO.md` file.

### Installation

This software is intended to work with recent version of GHC without requiring additional external dependencies, i.e. only relying on the libraries bundled with GHC.
Currently, GHC 9.2 and 9.4 are known to work, newer versions may works as well (if not, patches are more than welcome).
If a compatible GHC version is available on your system, mach can be installed using [Cabal][cabal web].
Otherwise, installation using [Guix][guix web] is recommended.
More details on both installation methods is provided below.

#### Cabal

Assuming the presence of a supported Haskell toolchain version, mach can be installed using Haskell's Cabal package manager.
In order to do so, clone the repository and running the following command from the repository root:

    $ cabal install

If Cabal is configured correctly, the mach executable show be available in your `$PATH`.

#### Guix

If a supported GHC version is not available on your system (e.g. because the GHC packaged by your distribution is too old) then you can also install mach using [Guix][guix web].
Guix is a functional [nix][nix web]-like package manager which can be used alongside your distributions package manger.
Once Guix is installed on your system, you can build mach by cloning the repository and running the following command:

    $ guix time-machine -C channels.scm -- package -f guix.scm

If Guix is configured correctly, this should add the mach executable to your user's profile.

### Development

Code should be formatted using [ormolu][ormolu github].
A githook for this purpose is available which can be enabled using:

    $ git config --local core.hooksPath .githooks

### Related Work

* [The original Make paper by Stuart I. Feldman][feldman make]
* [Plan 9's rewrite of make, called mk][plan9 mk]
* [A re-implementation of Plan 9 mk in Haskell][hmk github]
* [A Linux port of NetBSD make][bmake web]
* [The make implementation provided by the GNU project][gnu make]
* [remake, an enhanced version of GNU make][remake github]
* [pdpmake, a implementation of POSIX make written from scratch in C][pdpmake web]

### License

This work is licensed under [CC BY-NC-SA 4.0][cc license].

[posix draft]: https://www.opengroup.org/austin/login.html
[cc license]: http://creativecommons.org/licenses/by-nc-sa/4.0
[feldman make]: https://doi.org/10.1002/spe.4380090402
[plan9 mk]: https://plan9.io/sys/doc/mk.pdf
[hmk github]: https://github.com/mboes/hmk
[bmake web]: http://www.crufty.net/help/sjg/bmake.html
[gnu make]: https://www.gnu.org/software/make
[remake github]: https://github.com/rocky/remake
[pdpmake web]: https://frippery.org/make/
[pdpmake makefile]: https://github.com/rmyorston/pdpmake/blob/master/Makefile
[guix web]: https://guix.gnu.org
[nix web]: https://nixos.org/nix/
[cabal web]: https://haskell.org/cabal
[bazel web]: https://bazel.build
[bazel sandbox]: https://bazel.build/docs/sandboxing
[wiktionary machen]: https://en.wiktionary.org/wiki/machen
[ormolu github]: https://github.com/tweag/ormolu
