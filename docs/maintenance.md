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
