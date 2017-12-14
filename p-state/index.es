import _ from 'lodash'
import { ensureDirSync, readJsonSync, writeJsonSync } from 'fs-extra'
import { join } from 'path-extra'


const latestDataVersion = 'p-state-0.3.4'
/*
   state persistence: the following paths are kept and restored at runtime:

   - extStore.ui
   - extStore.gameUpdate

 */
const stateToPState = ({ui, gameUpdate}) => ({
  ui,
  gameUpdate,
  $dataVersion: latestDataVersion,
})

const getPStateFilePath = () => {
  const {APPDATA_PATH} = window
  const path = join(APPDATA_PATH,'navy-album')
  ensureDirSync(path)
  return join(path,'p-state.json')
}

const savePState = pState => {
  const path = getPStateFilePath()
  try {
    writeJsonSync(path,pState)
  } catch (err) {
    console.error('Error while writing to p-state file', err)
  }
}

const updatePState = oldPState => {
  if (oldPState.$dataVersion === latestDataVersion)
    return oldPState
  // eslint-disable-next-line prefer-const
  let newPState = oldPState
  if (newPState.$dataVersion === 'p-state-0.0.1') {
    _.set(newPState,'ui.shipsAlbum.searchText','')
    _.set(newPState,'ui.equipmentsAlbum.searchText','')
    newPState.$dataVersion = 'p-state-0.2.0'
  }

  if (newPState.$dataVersion === 'p-state-0.2.0') {
    _.set(newPState,'ui.shipsAlbum.shipViewer.dockingCurrentHp',1)
    newPState.$dataVersion = 'p-state-0.3.4'
  }

  if (newPState.$dataVersion === latestDataVersion) {
    setTimeout(() => savePState(newPState))
    return newPState
  }

  throw new Error('failed to update the config')
}

const loadPState = () => {
  try {
    return updatePState(readJsonSync(getPStateFilePath()))
  } catch (err) {
    if (err.syscall !== 'open' || err.code !== 'ENOENT') {
      console.error('Error while loading config', err)
    }
  }
  return null
}

export { stateToPState, savePState, loadPState }
