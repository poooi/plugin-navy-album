This document describes required maintenance to some assets:

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

## `assets/map-bgms.json`

We'll need to maintain a list of known map bgm ids.

```javascript
for (let i = 1; i < 200; ++i) {
    bac.requestBgm('map',i)
}
_.sortBy(_.keys(getStore().ext['poi-plugin-navy-album']._.swfCache.mapBgm).map(Number), x => x)
```

## ship upgrade info

Update `selectors/ship-upgrades.es` using info from `Core.swf` if needed
