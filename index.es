import { join } from 'path-extra'
import { readJsonSync } from 'fs-extra'
import { NavyAlbumRoot as reactClass } from './ui'
import { loadPState } from './p-state'
import { globalSubscribe, globalUnsubscribe } from './observers'
import { loadMasterDataFile } from './store/ext-root/master'
import {
  reducer,
  boundActionCreators as bac,
  initState,
} from './store'
import { register as registerIpc } from './ipc'

const windowMode = true

let unregisterIpc = null

const defDebuffInfo = readJsonSync(join(__dirname,'assets','default-debuff-info.json'))

const pluginDidLoad = () => {
  globalSubscribe()

  if (unregisterIpc !== null) {
    console.warn(`unregisterIpc should be null while getting ${unregisterIpc}`)
    if (typeof unregisterIpc === 'function') {
      try {
        unregisterIpc()
      } finally {
        unregisterIpc = null
      }
    }
  }
  unregisterIpc = registerIpc()
  setTimeout(() => {
    // start loading p-state
    let newUiState = {}
    let newGameUpdate = initState.gameUpdate
    let newDebuffInfo = defDebuffInfo
    try {
      const pState = loadPState()
      if (pState !== null && ('ui' in pState))
        newUiState = pState.ui
      if (pState !== null && ('gameUpdate' in pState))
        newGameUpdate = pState.gameUpdate
      if (pState !== null && ('debuffInfo' in pState))
        newDebuffInfo = {
          ...newDebuffInfo,
          ...pState.debuffInfo,
        }
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
      bac.debuffInfoModify(() => newDebuffInfo)

      setTimeout(() => {
        const mstData = loadMasterDataFile()
        bac.masterLoadFile(mstData)
      })
    }
  })
}

const pluginWillUnload = () => {
  if (typeof unregisterIpc !== 'function') {
    console.error(`unexpected unregisterIpc value: ${unregisterIpc}`)
  } else {
    try {
      unregisterIpc()
    } finally {
      unregisterIpc = null
    }
  }
  globalUnsubscribe()
}

export {
  reducer,
  pluginDidLoad,
  pluginWillUnload,

  reactClass,
  windowMode,
}
