# Abaks

Personal bank reconciliation statements app. Track income/expenses over named
periods and reconcile against expected vs. actual balances.

## Tech Stack

- **Language:** Haskell (GHC: nixpkgs default via Nix flake)
- **Build:** Cabal via Nix flake
- **Effect system:** Polysemy (+ polysemy-plugin)
- **Web API:** Servant (server, client, openapi3)
- **Serialization:** Aeson
- **Persistence:** Event sourcing — JSON file (`abaks-aggregates.json`)
- **Testing:** Hspec (+ hspec-discover)
- **Dev tooling:** Nix flake, direnv, ormolu, HLS, ghcid

## Build & Test Commands

```bash
cabal build all
cabal test all
cabal test abaks-test --test-options="-m <pattern>"
ormolu --mode inplace <file>      # format
```

To run the API server:

```bash
cabal run abaks-api
# listens on 0.0.0.0:8080, persists to abaks-aggregates.json
```

## Architecture

Clean / hexagonal style. Layers (bottom = pure domain, top = impure):

```
Entities (domain)              src/Abaks/Entities.hs
  Events, command handlers, pure validation
  ^
UseCases (application)         src/Abaks/UseCases.hs
  Polysemy effects wiring commands to the event store
  ^
InterfaceAdapters/API          src/Abaks/InterfaceAdapters/API.hs
  Servant routes, DTOs, semantic-error -> HTTP-status mapping
  ^
Driver (main)                  api/Main.hs
  Warp server on :8080, file-backed event store
```

### Modules

- `Abaks.Entities` — Domain: `AbaksEvent`, `Entry`, `PeriodId`, `Amount`,
  `EntryState`, `SemanticError`, pure command handlers (`startPeriod`,
  `addEntry`, `changeAmountEntry`, `validateEntry`, `commentEntry`,
  `markInConflictEntry`, `deleteEntry`).
- `Abaks.UseCases` — Polysemy use cases wrapping the command handlers with the
  `EventSourceEffect` and `Random` effects.
- `Abaks.InterfaceAdapters.API` — Servant `API` (NamedRoutes), request/response
  DTOs, `genericUseCaseHandler` mapping `SemanticError` to `ServerError`.
- `Abaks.Utils.EventSourcing` — `EventSourceEffect`, `CommandHandler` type,
  `runMemoryUnsafe` (tests) and `runFile` (production JSON file + MVar lock).
- `Abaks.Utils.Random` — `Random` effect (UUID V4) for period IDs.
- `Abaks.Utils.Servant` — `OperationId` combinator for OpenAPI operation IDs,
  `DerivingAPISum` for kebab-cased sum-type JSON encoding.

### Key design decisions

- **Event sourcing** — aggregates stored as event lists. Two interpreters:
  `runMemoryUnsafe` for tests, `runFile` for production.
- **Custom `OperationId` Servant combinator** injects operation IDs into
  OpenAPI specs transparently.
- **`DerivingAPISum` via-pattern** for kebab-cased sum-type JSON encoding
  (e.g. `EntryStateA`).
- **Semantic error -> HTTP status**: `MissingError` -> 404,
  `DisappearedError` -> 410, `InvalidError` -> 400,
  `BrokenInvariantError` -> 500, `PreconditionError` -> 412.

## Domain Model

- **Period** — has `PeriodId` (UUID), name, date range `[from, to]`, initial
  balance. Cannot be started twice.
- **Entry** — `EntryId` (Int), amount (cents), category, comment, state, date
  (must be within the period's date range).
- **EntryState** — `Expected | Unexpected | Validated | InConflict Text`.
- **Events** — `Started`, `EntryAdded`, `EntryAmountChanged`,
  `EntryValidated`, `EntryCommented`, `EntryMarkedInConflict`, `EntryDeleted`.
- Deleted entries are tracked as `Left ()` in the entry map and rejected by
  subsequent commands.

## Testing

- `test/Abaks/EntitiesSpec.hs` — Pure command-handler tests (7 scenarios).
- `test/Abaks/UseCasesSpec.hs` — Integration test running the full use-case
  chain through `runMemoryUnsafe`.
- `test/Spec.hs` — hspec-discover entry point.

When adding domain logic, add a corresponding entity spec. When adding a use
case, add a use-case spec using the `withFixture` pattern.

## API Endpoints

| Method | Path | Operation |
|--------|------|-----------|
| POST | `/` (body) | `createPeriod` |
| POST | `/:periodId` (body) | `createEntry` |
| PUT | `/:periodId/entry/:entryId/validate` | `validateEntry` |
| PUT | `/:periodId/entry/:entryId/markInConflict` (body) | `markInConflictEntry` |
| PUT | `/:periodId/entry/:entryId/comment` (body) | `commentEntry` |
| PUT | `/:periodId/entry/:entryId/amount` (body) | `changeAmountEntry` |
| PUT | `/:periodId/entry/:entryId` (body) | `deleteEntry` |

## Frontend

Not implemented. A static HTML/CSS mock lives in `mocked-ui/` describing the
target UI (left period menu, right striped statements table, bottom entry form
with state-based row coloring).

## Known Gaps / Roadmap (from `TODO`)

- [ ] Test view
- [ ] UI (real frontend)
- [ ] Multiple accounts
- [ ] Recurring entries
- [ ] Read endpoints (list periods / list entries) — currently write-only
- [ ] DB backend (currently file-based only)

## Conventions

- Default extensions are set in `abaks.cabal` (`DataKinds`, `GADTs`,
  `LambdaCase`, `OverloadedRecordDot`, `OverloadedStrings`,
  `RecordWildCards`, `TypeApplications`, etc.) — do not add LANGUAGE pragmas
  for these.
- Use `OverloadedRecordDot` for record field access (e.g. `req.name`).
- Use `LambdaCase` for single-argument pattern matching.
- Use `(<>)` over `(++)`.
- Qualified imports for `Data.Map`, `Data.Text`, `Data.UUID`.
- Use `Text` over `String`.
- Explicit export lists in all module declarations.
- Amounts are stored as `Int` cents, not floating point.
