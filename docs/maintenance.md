This document describes required maintenance to some assets:

## WhoCallsTheFleet

Provides stock equipment info and data necessary
to compute level-dependent stats (i.e. LoS, evasion, ASW).

```shell
npm run update-wctf
```

## kancolle-replay

Provides abyssal info.

```shell
npm run update-kcreplay
```

## `assets/default-digest.json`

For detecting changes to some parts of `getStore().const`.

Paste the result of the following command to that file.

```javascript
JSON.stringify(getStore().ext['poi-plugin-navy-album']._.gameUpdate.digest)
```

Note that this needs to be done before game maintenance (especially before events)
so new users will receive game update of the on-going event.

## `assets/ship-upgrades.json`

Old versions of poi provide no or incorrect (my fault xD) information
about ship upgrades. This asset is used as a fallback if what's
in the store isn't correct.

This needs to be updated when new remodelings are implemented.

First run the following code in console:

```javascript
Array.isArray(getStore().const.$shipUpgrades)
```

Proceed only when it returns `true`.

Run the following code and paste the result to that file.

```javascript
JSON.stringify(getStore().const.$shipUpgrades.filter(x => x.api_current_ship_id !== 0))
```
