This document describes caching mechanism of processed SWF files.

# Runtime representation

Data are stored under `<poi-plugin-navy-album>.swfDatabase` of redux,
which is an `Object` that has the following shape:

- `shipDb`: an `Object` that stores key-value pairs

    - key: masterId

        NOTE: be careful that masterId in ship graph is not necessarily present
        in `api_mst_ships`.

    - value: an Object:

        - `sgFileName` and `sgVersion`: the corresponding fields in
          `api_mst_shipgraphs`

        - `images`: an Object whose keys are `characterId`s and values
          whatever can be filled in `src` attribute of an `<img>` tag.

        - `lastFetch`: timestamp of last fetching of this resource, used for
          data invalidation or perhaps for applying a limit on how many runtime
          images should be kept in memory

        - `imagesDebuffed`: same structure as `images`, only makes sense on abyssal ships.
          When there's a debuffing mechanism for that abyssal ship, a swf resource
          with same filename but postfixed with `_d` will be present. and `imagesD`
          is used as the storage for that resource.

- `fetchLocks`: an Array of paths that we are currently fetching.
  Before actually initiate a `fetch` this lock must be checked.

- `diskFiles`: `files` of `index.json` (See below)

- `diskFilesReady`: indicates whether `diskFiles` is initialized.

# Representation on Disk

- Directory: `<config>/navy-album/cache`.

- `ship-<masterId>.json`: exactly the same as the corresponding values in `shipDb` of runtime.

- `index.json`: indexing file of files on disk, an JSON `Object`.

    - `files`: an Objects of the following structure:

        - key: `masterId`
        - value: `{sgFileName,sgVersion,lastFetch}`

      This file is to be loaded when initializing the plugin and is designed to
      have everything except `images` and `imagesDebuffed`, which might be expensive
      to load upon initialization.
      When a resource is being queried, `diskFiles` (runtime-representation of this part)
      is checked first, start loading process of the actual file on a cache hit.

    - `version`: for now always `cache-0.0.1`

# Cache Lookup Behavior

Upon requesting a resource:

1. attempt accessing `swfDatabase.shipDb[mstId]`, return on success.

2. attempt accessing `swfDatabase.diskFiles[mstId]`.
   On success, attempt requiring a lock (NOP on failure) then start loading the actual file,
   relase the lock and return.

3. attempt fetching as usual

4. an observer should monitor keys in `shipsDb`
   and maintain the representation on disk accordingly.

# TODO: Caching mechanism overhaul

Disk File Strucutre:

- root dir: `$APPDATA/poi/navy-album/cache/`

- metadata: `$APPDATA/poi/navy-album/cache/index.json`, which is an Object of the following shape

```
{
    ship: <ShipCache>,
    portBgm: <PortBgmCache>,
    mapBgm: <MapBgmCache>,
    dataVersion: <version>,
}
```

Redux Structure: `<store>.swfCache`, without `dataVersion`.
Implementation are free to choose between `Map`, `Object` and perhaps something else
for runtime representation as long as it's consistent throughout the plugin.

Additionally, `<store>.swfCache.fetchLocks` should be present at runtime as an Array of unique strings,
every element of `fetchLocks` takes one of the following form:

- `ship-<mstId>` or `ship-<mstId>_d`
- `portBgm-<pBgmId>`
- `mapBgm-<mBgmId>`

whose meanings are obivious.

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

## Port BGM Cache

`PortBgmCache` is an Object:

```
{
    [<port BGM id>]: {
        lastFetch: <date int>,
    },
}
```

- we expect single MP3 file with `soundId=1` from SWF extraction.
- the file is located: `<root>/portBgm/<pBgmId>.mp3`

## Map BGM Cache

`MapBgmCache` is an Object:

```
{
    [<map BGM id>]: {
        lastFetch: <date int>,
    },
}
```

- we expect single MP3 file with `soundId=1` from SWF extraction.
- the file is located: `<root>/mapBgm/<mBgmId>.mp3`
