Solving advent of code 2024 in clojure.

From the repl:

```clojure
❯ clj
Clojure 1.12.0
user=> (doto 'day02 require in-ns)
day02
day02=> (solve-a)
334
day02=> (solve-a sample-input)
2
day02=> (solve {:input "src/day02.txt"})
334
"Elapsed time: 14.854875 msecs"
400
"Elapsed time: 37.933125 msecs"
nil
```

From the clj:

```shell
❯ clj -X day02/solve
334
"Elapsed time: 11.596042 msecs"
400
"Elapsed time: 21.585833 msecs"
```
