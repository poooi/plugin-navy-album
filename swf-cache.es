import _ from 'lodash'
import { join } from 'path-extra'
import {
  ensureDirSync,
  writeJsonSync,
  readJsonSync,
  removeSync,
  statSync,
} from 'fs-extra'
import { modifyObject } from 'subtender'

/*
   ensureDirSync is costly to call, so instead we rely on the assumption that
   user does not manipulate the cache at runtime, therefore ensureDirSync call on
   any path should be sufficient
 */
const lazilyEnsureDirSync = _.memoize(path => ensureDirSync(path))

const getRootPath = () => {
  const {APPDATA_PATH} = window
  const path = join(APPDATA_PATH,'navy-album','cache')
  lazilyEnsureDirSync(path)
  return path
}

const getIndexFilePath = () =>
  join(getRootPath(), 'index.json')

/*
   subdir: 'ship'
 */
const getSubdirPath = subdir => {
  const path = join(getRootPath(), subdir)
  lazilyEnsureDirSync(path)
  return path
}

/*
   mstIdX: <mstId> or <mstId>_d
 */
const getShipFilePath = mstIdX => fileName => {
  const path = join(getSubdirPath('ship'), String(mstIdX))
  lazilyEnsureDirSync(path)
  return join(path, fileName)
}

const latestVersion = 'cache-1.0.0'

const saveSwfCache = swfCache => {
  try {
    const swfCacheWithVer = {
      ...swfCache,
      version: latestVersion,
    }
    writeJsonSync(getIndexFilePath(), swfCacheWithVer)
  } catch (err) {
    console.error(`failed to save cache index`, err)
  }
}

const fileExists = path => {
  try {
    statSync(path)
    return true
  } catch (_e) {
    return false
  }
}

/*
   verify that files in swfCache actually exists,
   and remove non-existing items from cache
 */
const verifySwfCache =
  modifyObject(
    'ship',
    // for ship swf cache, concatMap by pairs
    _.flow(
      _.toPairs,
      // Array<[k,v]> => Array<[k, v]>
      pairs => _.flatMap(pairs, pair => {
        const [mstIdX, record] = pair
        const getFP = getShipFilePath(mstIdX)
        const {files} = record
        // test files' existence and keep only exist ones
        const newFiles = _.fromPairs(
          _.flatMap(
            _.toPairs(files),
            fPair => {
              const [_ignored, fileName] = fPair
              const fp = getFP(fileName)
              return fileExists(fp) ? [fPair] : []
            }
          )
        )
        if (_.isEmpty(newFiles)) {
          // empty records are removed regardless of changes
          return []
        }
        if (_.isEqual(newFiles, files)) {
          // no diff, keep.
          return [pair]
        } else {
          // some files might be removed, but not all of them
          // keep new records for the time being
          return [[/* key */mstIdX, /* value */{...record, files: newFiles}]]
        }
      }),
      _.fromPairs,
    )
  )

const updateSwfCache = oldSwfCache => {
  if (!oldSwfCache)
    return null
  let curSwfCache = oldSwfCache

  /*
     0.0.1 => 0.5.0
     total cache overhaul, old data doesn't worth the effort of recovering
     so here we simply remove old cache dir and call it done
   */
  if (curSwfCache.version === 'cache-0.0.1') {
    const rootPath = getRootPath()
    removeSync(rootPath)
    ensureDirSync(rootPath)
    curSwfCache = {
      ship: {},
      portBgm: {},
      mapBgm: {},
      version: 'cache-0.5.0',
    }
  }

  /*
     0.5.0 => 1.0.0
   */
  if (curSwfCache.version === 'cache-0.5.0') {
    const {
      portBgm: _ignored1,
      mapBgm: _ignored2,
      ship,
    } = curSwfCache
    delete curSwfCache.portBgm
    delete curSwfCache.mapBgm

    const newSwfCache = {
      ship: {},
      version: 'cache-1.0.0',
    }
    _.keys(curSwfCache.ship).forEach(mstId => {
      // we now only do swf extraction on abyssal ship,
      // so we might as well just keep the abyssal part.
      if (mstId > 1500)
        newSwfCache.ship[mstId] = ship[mstId]
    })
    curSwfCache = newSwfCache
  }

  if (curSwfCache.version === latestVersion) {
    curSwfCache = verifySwfCache(curSwfCache)

    if (curSwfCache !== oldSwfCache) {
      setTimeout(() => saveSwfCache(curSwfCache))
    }
    const {version: _ignored, ...actualSwfCache} = curSwfCache
    return actualSwfCache
  }

  console.error(`failed to update cache version`)
  return null
}

const loadSwfCache = () => {
  try {
    return updateSwfCache(readJsonSync(getIndexFilePath()))
  } catch (err) {
    if (err.syscall === 'open' && err.code === 'ENOENT') {
      // file does not exist.
      return null
    } else {
      console.error(`error while loading cache index`, err)
    }
  }
  return null
}

export {
  getShipFilePath,
  saveSwfCache,
  loadSwfCache,
}
