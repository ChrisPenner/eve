# Change Log
Use the following sections:
- `Added`
- `Changed`
- `Deprecated`
- `Removed`
- `Fixed`
- `Security`

## 0.1.5
### Added
- `dispatchLocalEvent`, `addLocalListener`, `removeLocalListener`
     These local versions explicitly affect events in over the current Action's scope.

### Changed
- `dispatchEvent`, `addListener`, `removeListener`
    These now operate ONLY on the global level, and thus only accept `App` types.
    Migration: change existing `dispatchEvent` on `Actions` to `dispatchLocalEvent`
    (and similar for addListener and removeListener) 
