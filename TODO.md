# POSIX Compatibility

* [ ] Support for command-line options mandated by POSIX
    * [x] Basic integration with `System.Console.GetOpt`
    * [x] Support for specifying targets on the command-line
    * [x] Support for specifying assignments on the command-line
    * [x] Support for the `-f` flag
    * [ ] Support for all other flags
* [ ] Support for inference rules
    * [x] Parser support
    * [x] Handling of ".s2.s1" inference rules
    * [x] Respect and use `.SUFFIXES" (see *special targets* below)
    * [x] Handling of ".s1" inference rules
    * [ ] Tests
* [x] Support for command prefixes (`-`, `@`, `+`)
    * [x] Preliminary support for `@` and `-`
    * [ ] Support for `-`
    * [ ] Support for `.SILENT` / `.IGNORE` / `…` (see *special targets* below)
    * [ ] Tests
* [ ] Proper support for escaped newlines in commands and assignments
* [ ] Support for macro assignment operators other than `:=`
* [x] Support for macro expansions of the form `$(string1:subst1=[subst2])`
    * [ ] Support for nested expansions in `string1`
* [ ] Support for special targets (e.g. `.PHONY`)
* [ ] Support for internal macros (e.g. `$@`, `$%`, …)
* [x] Support for default rules (e.g. for C compilation)
* [ ] Support for environment variables (e.g. `MAKEFLAGS`)
* [ ] Support for libraries (prerequisites with parentheses)
