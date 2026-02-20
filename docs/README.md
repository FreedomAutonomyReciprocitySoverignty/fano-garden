# docs/ GitHub Pages Site

This folder is the consumer-facing static site published to GitHub Pages.

## Deployment

- Workflow: `.github/workflows/pages.yml`
- Publish source: `docs/`
- Expected URL form: `https://<org>.github.io/fano-garden/`
- `.nojekyll` is present so static assets are served without Jekyll transforms.

## Content Sources

- Runtime onboarding: `runtime/browser-v1/README.md`
- Compatibility notes: `runtime/posix-haskell/README.md`
- Normative spec: `dev-docs/reference-implementation-browser-v1.md`
- Glossary: `dev-docs/reference-implementation-glossary.md`
- NDJSON schemas: `dev-docs/reference-implementation-schemas.ndjson.md`
- Test plan: `dev-docs/reference-implementation-test-plan.md`

## Demo Snapshot Model

Published demo runtime code is copied into `docs/assets/demo/` from `runtime/browser-v1/`.
The source of truth remains `runtime/browser-v1/`.

### Snapshot Files

- `runtime.js`
- `identity.js`
- `commit.js`
- `invariants.js`
- `merge.js`
- `projection.js`
- `svgParser.js`
- `canonicalize.js`
- `constants.js`
- `ndjson.js`
- `index.js`
- `webrtc.js`
- `app.js` (docs-specific import/path adjustments)

Seed geometry is copied to `docs/assets/data/fano-garden.svg` from `dev-docs/artifacts/fano-garden.svg`.

## Update Checklist

1. Update runtime source files in `runtime/browser-v1/`.
2. Copy changed runtime modules into `docs/assets/demo/`.
3. Re-apply docs-specific path/import adjustments in `docs/assets/demo/app.js` if needed.
4. Re-copy `dev-docs/artifacts/fano-garden.svg` to `docs/assets/data/fano-garden.svg` if the artifact changed.
5. Run `scripts/check-docs-demo-sync.sh` and resolve any reported drift.
6. Preview locally from `docs/` before pushing.
