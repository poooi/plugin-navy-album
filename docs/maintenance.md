This document describes required maintenance to some assets.

Update to some of the assets are now semi-automated, to update those assets (requires [Stack](https://haskellstack.org/)):

```sh
# at project root
export NAVY_ALBUM_REPO=$(pwd)
cd scripts/kc-navy-album-maintenance
# 'def' action runs commonly used maintenance routines.
# see below
stack build && stack exec -- demo def
```

To only update individual assets:

- `assets/map-bgms.json`: run `demo map-bgms`
- `assets/default-digest.json`: run `demo default-digest`
- `assets/abyssal.json`: run `demo update-kcreplay`

## ship upgrade info

Update `selectors/ship-upgrades.es` using info from `kcs2/js/main.js` if needed

## graphs for special attacks

(Note: skip this step for now - `special` seems to exist for all ships that have special attacks,
despite some are just using normal CGs)

Update `ui/ships-album/ship-viewer/gallery-view.es` using info from running `demo scan-special-ship-graphs` if needed
(note that this sub-command does not run by `def`).

## instant build and dev mat cost

Update asset `assets/remodel-info-useitem.json` if needed.

`stack build && stack exec -- demo build-remodel-useitem-consumption` should get this computed correctly,
but it'll require an extra tool set as `REMODEL_COST_CALCULATOR` environment variable.
