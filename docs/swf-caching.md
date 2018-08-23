This document describes caching mechanism of processed SWF files.

# Disk File Strucutre

- root dir: `$APPDATA/poi/navy-album/cache/`

- metadata: `$APPDATA/poi/navy-album/cache/index.json`, which is an Object of the following shape

```
{
    ship: <ShipCache>,
    version: '0.5.0',
}
```

Redux Structure: `<store>.swfCache`, without `version`.
Implementation are free to choose between `Map`, `Object` and perhaps something else
for runtime representation as long as it's consistent throughout the plugin.

Additionally, `<store>.swfCache.fetchLocks` should be present at runtime as an Array of unique strings,
every element of `fetchLocks` is a path, which usually takes the form of:

`/kcs/resources/...`

## Ship Cache

`ShipCache` is an Object whose keys are strings of `<mstId>` or `<mstId>_d` for debuffed ships,
and values:

```
{
    lastFetch: <date int>,
    sgFileName: <string>,
    sgVersion: <string>,
    files: <Object of characterId to filename>,
}
```

- a `<date int>` stands for evaluation result of `Number(<Date>)`.
- prefix `sg` stands for ship graph, `sgFileName` and `sgVersion` both come from master data.
- example of files: `{1: "1.png", 2: "2.jpg", 5: "5.png"}`
- cached files are located under: `<root>/ship/<mstId>/`
- for abyssal ships, if "debuffed" form exists, files will be under `<root>/ship/<mstId>_d/` and
  `ShipCache` will have keys like `<mstId>_d`.
- it's assumed that user will never remove or modify cache dir at runtime, so if user happens to
  remove or change some part of the cache, this plugin must be restarted to sync with the file system.
- file existence is tested before loading, and missing files must be removed from `files` field
  accordingly.
