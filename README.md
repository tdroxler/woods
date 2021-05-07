Woods
=====

Minimal univesal LSP server.
 - Listening [sari](https://github.com/aloiscochard/sarsi) for diagnostics
 - Using [semanticdb](https://scalameta.org/docs/semanticdb/guide.html) and `tags` (generated for example with [ctags](http://ctags.sourceforge.net/)) for jump-to-definition

Support:
* [textDocument/publishDiagnostics](https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#textDocument_publishDiagnostics)
* [textDocument/definition](https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#textDocument_definition)
* [textDocument/references](https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#textDocument_references)


Build on Nix
--------------

`nix-shell --pure --command "cabal build"`
