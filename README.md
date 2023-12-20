# mach

A WiP implementation of `make(1)`, aims to be compatible with [P1003.1™-202x/D3][posix draft] at some point.

## Status

Proof of concept, currently largely untested, buggy, and incomplete.

## Installation

This software is designed to work with GHC 9.4.7 without requiring any external dependency, i.e. only relying on the bundled library versions.
If this GHC version is installed, mach can be installed by cloning the repository and running the following command from the repository root:

    $ cabal install

## Related Work

* [The original Make paper by Stuart I. Feldman][feldman make]
* [Plan 9's rewrite of make, called mk][plan9 mk]
* [A re-implementation of Plan 9 mk in Haskell][hmk github]
* [A Linux port of NetBSD make][bmake web]
* [The make implementation provided by the GNU project][gnu make]
* [remake, an enhanced version of GNU make][remake github]
* [pdpmake, a implementation of POSIX make written from scratch in C][pdpmake web]

## License

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
