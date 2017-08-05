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

    - `version`: for now always `initial` (TODO to keep sync with package version on release)

# Cache Lookup Behavior

(TODO: impl)
(TODO: impl data invalidation & cache limit after other stuff are implemented)

Upon requesting a resource:

1. attempt accessing `swfDatabase.shipDb[mstId]`, return on success.

2. attempt accessing `swfDatabase.diskFiles[mstId]`.
   On success, attempt requiring a lock (NOP on failure) then start loading the actual file,
   relase the lock and return.

3. attempt fetching as usual

4. an observer should monitor keys in `shipsDb`
   and maintain the representation on disk accordingly.
