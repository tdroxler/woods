Woods
=====

Minimal Scala LSP server that use sbt behind the scene.

Support:
* [textDocument/publishDiagnostics](https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#textDocument_publishDiagnostics) (on sbt compile)
* [textDocument/definition](https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#textDocument_definition)
* [textDocument/references](https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#textDocument_references)

Sbt needs to be configured the following:

For diagnostics, you need the sbt's server: `autoStartServer := true` and for definitions/references, you need some generated [semanticdb](https://scalameta.org/docs/semanticdb/guide.html) files:
```
addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.1.0" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
```
and/or a `tags` file generated for example with [ctags](http://ctags.sourceforge.net/). This `tags` file currently have to be named like this and being place in the root where you launch `woods`.

The `JumpToDefinition` first try to find the precise info with `semanticdb` files and fall-back on `tags` file. If you use plugins like [sbt-ctags](https://github.com/ceedubs/sbt-ctags),
you can also jump on dependencies (just rename/move the generated tags file to match the above requirement). You also need to have the `bin/tags-lsp` script in your `PATH`.

You can have a look the [example used for testing](https://github.com/tdroxler/woods/tree/master/test/resources)

Install on Nix
--------------

In your system configuration add this (replace the hash commit and sha256):

    woods = pkgs.haskellPackages.callPackage (builtins.fetchTarball {
      url = "https://github.com/tdroxler/woods/archive/191e7ad4793637fe1cbe35e9bee375a6de459cb6.tar.gz";
      sha256 = "1fjivqqiq5yfrpngfnmm4xk3ik9jxgnvn8dp2j6hymh81vdqqk4b";
    }) {};

You can get the `sha256` value with `nix-prefetch-url --unpack URL`

And then you can use `woods` in your nix configuration as any packages:

    environment.systemPackages = [ woods ];

This will build on the unstable channel. Stable has not been tested.
