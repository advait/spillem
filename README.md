# Spillem

A toy LISP implementation in [Elm](https://elm-lang.org/). Spillem is based on the wonderful [mal (Make a Lisp)](https://github.com/kanaka/mal) project. Spillem deviates from the mal spec due to the limitations of Elm (inability to read files, manipulate stdio, etc).

## Interpreter Features

- [x] Parser
- [x] Ints, Lists, Symbols
- [x] Eval
- [x] Environments
- [x] Functions/Lexical Closures
- [ ] Macros
- [ ] Atoms
- [ ] Quasi/Unquote
- [ ] Vectors
- [ ] Maps
- [ ] Try/Catch
- [ ] Meta-circular interpreter

## Repl
Spillem also comes bundled with an interactive repl in a web UI. To access this, run `elm reactor` and open `src/Repl.elm`.

## Why?
You will never truly understand a lexical closure until you have implemented one yourself. This project has helped me understand the nature of programming languages, interpreters, functional programming, etc. more than any other side project I have worked on.
