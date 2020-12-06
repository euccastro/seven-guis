7guis
=====

Clojurescript implementation of [7GUIs](https://eugenkiss.github.io/7guis/tasks/)

[Demo](https://euccastro.github.io/seven-guis/)

To run/tweak,

- [Install Clojure](https://clojure.org/guides/getting_started) if necessary, and
- run `clj -M -m figwheel.main -b <task>`

where `<task>` is one of

- `counter`
- `temperature-converter`
- `flight-booker`
- `timer`
- `crud`
- `circle-drawer`
- `cells`

To build once with advanced optimizations

`clojure -M -m figwheel.main -O advanced -bo <task>`

To run tests

`clojure -M:test`
