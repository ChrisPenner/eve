# Change Log
Use the following sections:
- `Added`
- `Changed`
- `Deprecated`
- `Removed`
- `Fixed`
- `Security`
- `Migration`

---

## 0.1.8
### Added
- `Action`s now have a Monoid instance.

## 0.1.7
### Changed
- `Dispatcher` is now called `EventDispatcher` to alleviate confusion.
- `liftApp` is now called `runApp`.
- `eve_` now has the simplified type `eve_ :: App () -> IO ()`

### Migration
- Rename all `Dispatcher` -> `EventDispatcher`
- Rename all `liftApp` -> `runApp`
 
---
## 0.1.6
### Added
-   `runActionOver`: This is equivalent to `runAction` in 0.1.5
-   `makeStateLens`: This simplifies creation of lenses which are usable in Actions.

### Changed
- `runAction` now assumes the `stateLens` by default, if you wish to specify a 

### Migration
- Change occurances of `runAction` to `runActionOver` and occurances of `runAction stateLens`
    to just `runAction`.
 
---
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
