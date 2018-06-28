Gimmerrors
=====

I use a lot [sarsi](https://github.com/aloiscochard/sarsi) for my daily scala coding and love it. With the growing microsoft's [Language Servre Protocol](https://microsoft.github.io/language-server-protocol/) and the new [sbt server](https://www.scala-sbt.org/1.x/docs/sbt-server.html) introduced in sbt 1.0, I wanted to make a tool to communicate between sbt and neovim (with [autozimu/LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim), using the LSP. The very good [metals](https://github.com/scalameta/metals) already do it, but it's a bit too heavy for me now.

What a good oportunity to learn Haskell by the way (probably the main reason of this project).

# Install

```
  cabal build
```

and in your `vimrc`, if you are using the above plugin:

```
  let g:LanguageClient_serverCommands = {
    \ 'scala': ['path_to_your_gimmerrors_executable']
    \ }
```

# TODO

- [ ] Extract sbt related stuff to have a language agnostic server
- [ ] Integrate with sarsi or similare to have other language diagnostics
