# ulp-runtime (POSIX/Haskell v1 scaffold)

Build:

```bash
cd runtime/posix-haskell
cabal build
```

Commands:

```bash
cabal run ulp-runtime -- init --root /tmp/ulp-demo
cabal run ulp-runtime -- commit --root /tmp/ulp-demo --type commit
cabal run ulp-runtime -- validate --root /tmp/ulp-demo
cabal run ulp-runtime -- merge --root /tmp/ulp-demo --from /tmp/remote.ndjson
cabal run ulp-runtime -- tip --root /tmp/ulp-demo
cabal run ulp-runtime -- replay --root /tmp/ulp-demo
```

Smoke test:

```bash
cd runtime/posix-haskell
./test/cli-smoke.sh
```

Cross-runtime vectors:

```bash
cd runtime/posix-haskell
./test/vector-check.sh          # browser fixture -> Haskell validate
./test/reverse-vector-check.sh  # Haskell-produced commit -> browser validate
```

Signature + replay interop suites:

```bash
cd runtime/posix-haskell
export ULP_TEST_PRIVKEY=0x0123456789012345678901234567890123456789012345678901234567890123
./test/sig-suite.sh
./test/replay-suite.sh
./test/interop-suite.sh
```

Note:
- Current signature suite uses deterministic test-only HMAC signer/verifier scripts (`test/sign-message.mjs`, `test/verify-signature.mjs`) to lock signing-message parity and failure modes without external dependencies.

This scaffold matches the browser-v1 compatibility contract:
- signing message: `merkle.root` else `self_hash`
- optional `lc`
- optional `merkle`
- deterministic merge/tip ordering: `lc`, then `t`, then lexical tie-break
