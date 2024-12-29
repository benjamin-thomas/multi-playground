## To further explore

```sh
cd Day06c
cabal run v1
```

This solution (not mine) runs in 800ms on my machine.

I think the difference with my approach is that I tried to represent the actual
loop, to eventually "visualize" it, but that seems to generates a lot of garbage
(GC), even using mutable arrays (which kills performance).

Compare with my C "brain dead" and nooby solution that runs in 500ms.
