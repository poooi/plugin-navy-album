import _ from 'lodash'
import { modifyObject } from 'subtender'
import { createSelector } from 'reselect'
import { ensureDirSync, readJsonSync, writeJsonSync } from 'fs-extra'
import { join } from 'path-extra'

import { uiSelector, gameUpdateSelector, debuffInfoSelector } from '../selectors'

const latestDataVersion = 'p-state-1.2.0'

/*
   state persistence: the following paths are kept and restored at runtime:

   - extStore.ui
   - extStore.gameUpdate

 */
const pStateSelector = createSelector(
  uiSelector,
  gameUpdateSelector,
  debuffInfoSelector,
  (ui, gameUpdate, debuffInfo) => ({ui, gameUpdate, debuffInfo})
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
        focus: {type: 'all'},
      },
    })
    newPState.$dataVersion = 'p-state-0.6.0'
  }
  if (newPState.$dataVersion === 'p-state-0.6.0') {
    /*
       digesting method updated at 1.0.0 to include all parts of api_version,
       which means we'll have to abandon old digest and allow new one to come in.
     */
    newPState = modifyObject(
      'gameUpdate',
      modifyObject('digest', () => null)
    )(newPState)
    newPState.$dataVersion = 'p-state-1.0.0'
  }

  if (newPState.$dataVersion === 'p-state-1.0.0') {
    newPState = {
      ...newPState,
      debuffInfo: {},
      $dataVersion: 'p-state-1.1.0',
    }
  }

  if (newPState.$dataVersion === 'p-state-1.1.0') {
    /*
       turns out the old way (prior to 0.6.0) of detection version change is correct:
       api_version[1] and api_version[2] are for voices (thank you, tanaka.)
       so here we need to nuke ship digests again.
     */
    newPState = modifyObject(
      'gameUpdate',
      modifyObject('digest', () => null)
    )(newPState)
    newPState.$dataVersion = 'p-state-1.2.0'
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


// note that path `gameUpdate.digest` of returned structure might be `null`,
// which means we must drop the old one and allow default one to come in.
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
