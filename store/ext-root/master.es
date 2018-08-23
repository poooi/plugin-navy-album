/*
   this reducer keeps track of master data from api_start2
 */
import _ from 'lodash'
import { join } from 'path-extra'
import {
  ensureDirSync,
  readJsonSync, writeJsonSync,
} from 'fs-extra'

const reducer = (state = null, action) => {
  if (action.type === '@@Response/kcsapi/api_start2/getData') {
    const {body} = action
    // only update when it's changed
    return _.isEqual(body, state) ? state : body
  }

  /*
     load from file stored previously.
     note that we always prefer the latest data from API,
     this means if current state is non-empty, this action will do nothing.
   */
  if (action.type === '@poi-plugin-navy-album@master@LoadFile') {
    const {data} = action
    if (!_.isEmpty(state) || _.isEmpty(data))
      return state

    return data
  }

  return state
}

const actionCreators = {
  masterLoadFile: data => ({
    type: '@poi-plugin-navy-album@master@LoadFile',
    data,
  }),
}

const getMasterDataFilePath = _.memoize(() => {
  const {APPDATA_PATH} = window
  const path = join(APPDATA_PATH,'navy-album')
  ensureDirSync(path)
  return join(path,'master.json')
})

const saveMasterDataFile = masterData => {
  try {
    writeJsonSync(getMasterDataFilePath(), masterData)
  } catch (err) {
    console.error(`failed to save master data file`, err)
  }
}

const loadMasterDataFile = () => {
  try {
    return readJsonSync(getMasterDataFilePath())
  } catch (err) {
    if (err.syscall === 'open' && err.code === 'ENOENT') {
      // file does not exist.
      return null
    } else {
      console.error(`error while loading master data file`, err)
    }
  }
  return null
}


export {
  reducer,
  actionCreators,

  saveMasterDataFile,
  loadMasterDataFile,
}
