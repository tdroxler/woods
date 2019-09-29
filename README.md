Woods
=====

Minimal Scala LSP server that use sbt behind the scene.

Support:
* [textDocument/publishDiagnostics](https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#textDocument_publishDiagnostics) (on sbt compile)
* [textDocument/definition](https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#textDocument_definition) (not for dependencies yet)

Sbt needs to be configured the following:

For diagnostics, you need the sbt's server: `autoStartServer := true` and for definitions, you need some generated [semanticdb](https://scalameta.org/docs/semanticdb/guide.html) files:
```
addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.1.0" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
```

You can have a look the [example used for testing](https://github.com/tdroxler/woods/tree/master/test/resources)
