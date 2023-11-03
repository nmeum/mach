# mach

An implementation of POSIX `make(1)` as defined in P1003.1™-202x/D3.

## About

To-Do.

## Roadmap

* [ ] Support for command-line options mandated by POSIX
* [ ] Support for inference rules
* [ ] Support for command prefixes (`-`, `@`, `+`)
* [ ] Proper support for escaped newlines in commands and assignments
* [ ] Support for macro assignment operators other than `:=`
* [ ] Support for macro expansions of the form `$(string1:subst1=[subst2])`
* [ ] Support for special targets (e.g. `.PHONY`)
* [ ] Support for internal macros (e.g. `$@`, `$%`, …)
* [ ] Support for specifying assignments on the command-line
* [ ] Support for default rules (e.g. for C compilation)
* [ ] Support for environment variables (e.g. `MAKEFLAGS`)

## Installation

To-Do.

## Usage

To-Do.

## Related Work

* [The original Make paper by Stuart I. Feldman][feldman make]
* [Plan 9's rewrite of make, called mk][plan9 mk]
* [A re-implementation of Plan mk in Haskell][hmk github]
* [A Linux port of NetBSD make][bmake web]
* [The make implementation provided by the GNU project][gnu make]
* [remake, an enhanced version of GNU make][remake github]

## License

This work is licensed under [CC BY-NC-SA 4.0][cc license].

[cc license]: http://creativecommons.org/licenses/by-nc-sa/4.0
[feldman make]: https://doi.org/10.1002/spe.4380090402
[plan9 mk]: https://plan9.io/sys/doc/mk.pdf
[hmk github]: https://github.com/mboes/hmk
[bmake web]: http://www.crufty.net/help/sjg/bmake.html
[gnu make]: https://www.gnu.org/software/make
[remake github]: https://github.com/rocky/remake
