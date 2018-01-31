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

const getRootPath = () => {
  const {APPDATA_PATH} = window
  const path = join(APPDATA_PATH,'navy-album','cache')
  ensureDirSync(path)
  return path
}

const getIndexFilePath = () =>
  join(getRootPath(), 'index.json')

/*
   subdir: 'ship' / 'portBgm' / 'mapBgm'
 */
const getSubdirPath = subdir => {
  const path = join(getRootPath(), subdir)
  ensureDirSync(path)
  return path
}

/*
   mstIdX: <mstId> or <mstId>_d
 */
const getShipFilePath = mstIdX => fileName => {
  const path = join(getSubdirPath('ship'), String(mstIdX))
  ensureDirSync(path)
  return join(path, fileName)
}

/*
   bgmType: 'port' / 'map'
 */
const getBgmFilePath = bgmType => bgmId => {
  const path = join(getSubdirPath(bgmType === 'port' ? 'portBgm' : 'mapBgm'))
  ensureDirSync(path)
  return join(path, `${bgmId}.mp3`)
}

const latestVersion = 'cache-0.5.0'

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

const mapValues = modifier => obj => _.flow(
  _.keys(obj).map(k =>
    modifyObject(k, modifier)
  )
)(obj)

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
const verifySwfCache = _.flow(
  modifyObject(
    'ship',
    mapValues((record, mstIdX) => {
      const getFP = getShipFilePath(mstIdX)
      const {files} = record
      const newFiles = _.fromPairs(
        _.flatMap(
          _.toPairs(files),
          pair => {
            const [_ignored, fileName] = pair
            const fp = getFP(fileName)
            return fileExists(fp) ? [pair] : []
          }
        )
      )
      if (_.isEqual(newFiles, files)) {
        return record
      } else {
        return {
          ...record,
          files: newFiles,
        }
      }
    })
  ),
  modifyObject('portBgm', _.identity),
  modifyObject('mapBgm', _.identity)
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
    removeSync(getRootPath())
    // call this again to make sure the directory is created afterwards
    getRootPath()
    curSwfCache = {
      ship: {},
      portBgm: {},
      mapBgm: {},
      version: 'cache-0.5.0',
    }
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
  getBgmFilePath,
  saveSwfCache,
  loadSwfCache,
}
