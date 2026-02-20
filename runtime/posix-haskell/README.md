# ulp-runtime (POSIX/Haskell v1 scaffold)

Commands:

```bash
cabal run ulp-runtime -- init --root /tmp/ulp-demo
cabal run ulp-runtime -- commit --root /tmp/ulp-demo --type commit
cabal run ulp-runtime -- validate --root /tmp/ulp-demo
cabal run ulp-runtime -- tip --root /tmp/ulp-demo
cabal run ulp-runtime -- replay --root /tmp/ulp-demo
```

This scaffold matches the browser-v1 compatibility contract:
- signing message: `merkle.root` else `self_hash`
- optional `lc`
- optional `merkle`
- deterministic merge/tip ordering: `lc`, then `t`, then lexical tie-break
