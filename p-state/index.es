import _ from 'lodash'
import { createSelector } from 'reselect'
import { ensureDirSync, readJsonSync, writeJsonSync } from 'fs-extra'
import { join } from 'path-extra'

import { uiSelector, gameUpdateSelector } from '../selectors'

const latestDataVersion = 'p-state-0.6.0'

/*
   state persistence: the following paths are kept and restored at runtime:

   - extStore.ui
   - extStore.gameUpdate

 */
const pStateSelector = createSelector(
  uiSelector,
  gameUpdateSelector,
  (ui, gameUpdate) => ({ui, gameUpdate})
)

const getPStateFilePath = () => {
  const {APPDATA_PATH} = window
  const path = join(APPDATA_PATH,'navy-album')
  ensureDirSync(path)
  return join(path,'p-state.json')
}

const savePState = pState => {
  const path = getPStateFilePath()
  try {
    const pStateWithVer = {
      ...pState,
      $dataVersion: latestDataVersion,
    }
    writeJsonSync(path,pStateWithVer)
  } catch (err) {
    console.error('Error while writing to p-state file', err)
  }
}

const updatePState = oldPState => {
  // eslint-disable-next-line prefer-const
  let newPState = oldPState
  if (newPState.$dataVersion === 'p-state-0.0.1') {
    newPState = _.cloneDeep(newPState)
    _.set(newPState,'ui.shipsAlbum.searchText','')
    _.set(newPState,'ui.equipmentsAlbum.searchText','')
    newPState.$dataVersion = 'p-state-0.2.0'
  }

  if (newPState.$dataVersion === 'p-state-0.2.0') {
    newPState = _.cloneDeep(newPState)
    _.set(newPState,'ui.shipsAlbum.shipViewer.dockingCurrentHp',1)
    newPState.$dataVersion = 'p-state-0.3.4'
  }

  if (newPState.$dataVersion === 'p-state-0.3.4') {
    newPState = _.cloneDeep(newPState)
    _.set(newPState, 'ui.musicLibrary', {
      activeTab: 'port',
      mapBgmViewer: {
        focus: {type: 'map', mapId: 11},
        listMode: 'map',
      },
    })
    newPState.$dataVersion = 'p-state-0.6.0'
  }

  if (newPState.$dataVersion === latestDataVersion) {
    if (oldPState !== newPState) {
      setTimeout(() => savePState(newPState))
    }

    const {$dataVersion: _ignored, ...actualPState} = newPState
    return actualPState
  }

  throw new Error('failed to update the config')
}

const loadPState = () => {
  try {
    return updatePState(readJsonSync(getPStateFilePath()))
  } catch (err) {
    if (err.syscall !== 'open' || err.code !== 'ENOENT') {
      console.error('Error while loading p-state', err)
    }
  }
  return null
}

export { pStateSelector, savePState, loadPState }
