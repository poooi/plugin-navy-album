import _ from 'lodash'
import { join } from 'path-extra'
import {
  ensureDirSync,
  writeJsonSync,
  readJsonSync,
} from 'fs-extra'

// delay dir path lookup, but when it's computed, it should stay consistent.
const getRootPath = _.memoize(() => {
  const {APPDATA_PATH} = window
  const path = join(APPDATA_PATH,'navy-album','cache')
  ensureDirSync(path)
  return path
})

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

export {
  getShipFilePath,
}
