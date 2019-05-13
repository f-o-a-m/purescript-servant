# purescript-servant
A servant like DSL for templating requests

## Intro
This library defines a type level DSL à la servant to describe the types of http requests and to implement clients.
It is expiremental, will probably change often or be merged with [purescript-trout](https://github.com/owickstrom/purescript-trout)

## Examples
There is currently only one example project which is open source, and that's [plasma-demo](https://github.com/f-o-a-m/plasma-demo).
That project is pretty narrow in scope -- it's a client library for a specific plasma-mvp implementation -- but it should be clear
how this library is being used. See the [Routes](https://github.com/f-o-a-m/plasma-demo/blob/master/src/Plasma/Routes.purs) module for instance.
There is also an end-to-end test suite for that project, so you can see examples for how to build config and use the functions.
