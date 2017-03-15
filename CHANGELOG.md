# Change Log
Use the following sections:
- `Added`
- `Changed`
- `Deprecated`
- `Removed`
- `Fixed`
- `Security`
- `Migration`

## 0.1.6
### Added
-   `runActionOver`: This is equivalent to `runAction` in 0.1.5


### Changed
- `runAction` now assumes the `stateLens` by default, if you wish to specify a 

### Migration
- Change occurances of `runAction` to `runActionOver` and occurances of `runAction stateLens`
    to just `runAction`.

## 0.1.5
### Added
- `dispatchLocalEvent`, `addLocalListener`, `removeLocalListener`
     These local versions explicitly affect events in over the current Action's scope.

### Changed
- `dispatchEvent`, `addListener`, `removeListener`
    These now operate ONLY on the global level, and thus only accept `App` types.

### Migration
-   Change existing `dispatchEvent` on `Actions` to `dispatchLocalEvent` (and
    similar for addListener and removeListener)
