import _ from 'lodash'
import { join } from 'path-extra'
import {
  ensureDirSync,
  writeJsonSync,
  readJsonSync,
} from 'fs-extra'

// delay dir path lookup, but when it's computed, it should stay consistent.
const getCacheDirPath = _.memoize(() => {
  const {APPDATA_PATH} = window
  const path = join(APPDATA_PATH,'navy-album','cache')
  ensureDirSync(path)
  return path
})

const getCacheFilePath = mstId => {
  const base = getCacheDirPath()
  return join(base,`ship-${mstId}.json`)
}

const writeCacheFile = (mstId, shipRecord) => {
  const filePath = getCacheFilePath(mstId)
  return writeJsonSync(filePath, shipRecord)
}

const readCacheFile = mstId => {
  const filePath = getCacheFilePath(mstId)
  return readJsonSync(filePath)
}

const writeIndexFile = diskFiles => {
  const base = getCacheDirPath()
  return writeJsonSync(
    join(base,'index.json'),
    {
      files: diskFiles,
      version: 'initial',
    }
  )
}

export {
  writeCacheFile,
  readCacheFile,

  writeIndexFile,
}
