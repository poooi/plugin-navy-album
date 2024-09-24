import _ from 'lodash'
import { join } from 'path-extra'
import { readJsonSync } from 'fs-extra'

import {} from './devtools'
import { NavyAlbumRoot as reactClass } from './ui'
import { loadPState } from './p-state'
import { globalSubscribe, globalUnsubscribe } from './observers'
import { loadMasterDataFile } from './store/ext-root/master'
import {
  reducer,
  boundActionCreators as bac,
  initState,
} from './store'
import { registerIpc, unregisterIpc } from './ipc'

const windowMode = true

const pluginDidLoad = () => {
  globalSubscribe()
  registerIpc()
  setTimeout(() => {
    // start loading p-state
    let newUiState = {}
    let newGameUpdate = initState.gameUpdate
    let newDebuffInfo = {}
    try {
      const pState = loadPState()
      if (pState !== null && ('ui' in pState))
        newUiState = pState.ui
      if (pState !== null && ('gameUpdate' in pState))
        newGameUpdate = pState.gameUpdate
      if (pState !== null && ('debuffInfo' in pState))
        newDebuffInfo = pState.debuffInfo
    } catch (e) {
      console.error('error while initializing', e)
    } finally {
      bac.uiReady(newUiState)

      // now make sure that we always have gameUpdate.digest available
      // before setting the ready flag
      if (!newGameUpdate.digest) {
        newGameUpdate.digest =
          readJsonSync(join(__dirname,'assets','default-digest.json'))
      }

      bac.gameUpdateReady(newGameUpdate)
      if (_.isEmpty(newDebuffInfo)) {
        newDebuffInfo = readJsonSync(join(__dirname,'assets','default-debuff-info.json'))
      }
      bac.debuffInfoModify(() => newDebuffInfo)

      setTimeout(() => {
        const mstData = loadMasterDataFile()
        bac.masterLoadFile(mstData)
      })
    }
  })
}

const pluginWillUnload = () => {
  unregisterIpc()
  globalUnsubscribe()
}

export {
  reducer,
  pluginDidLoad,
  pluginWillUnload,

  reactClass,
  windowMode,
}
