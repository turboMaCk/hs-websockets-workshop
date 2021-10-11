# WebSockets Server in Haskell

Simple example implementation of websockets server in haskell.

1. See [`Main.hs`](app/Main.hs) for instructions
2. Client code already [implemeted](index.html)
3. Nix and Stack ready
4. [Open live app](http://chat.planning-game.com)
5. Check [cheat branch](https://github.com/turboMaCk/hs-websockets-workshop/tree/cheat) for example implementation

## Nix

- you can use `cabal` within `nix-shell`
- to enable [direnv](https://direnv.net/) use `echo "use_nix" > .envrc && direnv allow`
- you can use `nix-build` to build project using nix

## Stack

- stack based workflow should be working. use `stack --help` to learn how to use it.

## Intro

Today we're building:

- simple steteful websockets based chat room
- no database
- using STM for state
- no wai/warp
- no MTL, tagless final etc. Beginner Haskell
- persistent sessions, multiple connections per session etc.

## More resources

- [Jasper's own introduction to websockets](https://jaspervdj.be/websockets/example/server.html)
- [servant-websockets](https://hackage.haskell.org/package/servant-websockets) for Servant projects
- [wai-websockets](https://hackage.haskell.org/package/wai-websockets) for any wai app
- [check hackage](https://hackage.haskell.org/packages/search?terms=websockets) for more
