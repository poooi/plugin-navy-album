This document describes required maintenance to some assets.

Update to some of the assets are now semi-automated, to update those assets (requires [Stack](https://haskellstack.org/)):

```sh
# at project root
export NAVY_ALBUM_REPO=$(pwd)
cd scripts/kc-navy-album-maintenance
# 'all' action run all maintenance routines.
# see below
stack build && stack exec -- demo all
```

To only update individual assets:

- `assets/map-bgms.json`: run `demo map-bgms`
- `assets/default-digest.json`: run `demo default-digest`
- `assets/abyssal.json`: run `demo update-kcreplay`

## ship upgrade info

Update `selectors/ship-upgrades.es` using info from `kcs2/js/main.js` if needed
