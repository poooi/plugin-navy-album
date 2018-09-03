import { join } from 'path-extra'
import { readJsonSync } from 'fs-extra'
import { NavyAlbumRoot as reactClass } from './ui'
import { loadPState } from './p-state'
import { globalSubscribe, globalUnsubscribe } from './observers'
import { loadSwfCache } from './swf-cache'
import { loadMasterDataFile } from './store/ext-root/master'
import {
  reducer,
  withBoundActionCreator,
  boundActionCreators,
  initState,
} from './store'
import { register as registerIpc } from './ipc'

const windowMode = true

let unregisterIpc = null

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
    try {
      const pState = loadPState()
      if (pState !== null)
        newUiState = pState.ui
      if (pState !== null)
        newGameUpdate = pState.gameUpdate
    } catch (e) {
      console.error('error while initializing', e)
    } finally {
      withBoundActionCreator(
        ({uiReady}) => uiReady(newUiState)
      )
      // now make sure that we always have gameUpdate.digest available
      // before setting the ready flag
      if (!newGameUpdate.digest) {
        newGameUpdate.digest =
          readJsonSync(join(__dirname,'assets','default-digest.json'))
      }
      withBoundActionCreator(
        ({gameUpdateReady}) => gameUpdateReady(newGameUpdate)
      )

      setTimeout(() => {
        const swfCache = loadSwfCache()
        boundActionCreators.swfCacheReady(swfCache)

        const mstData = loadMasterDataFile()
        boundActionCreators.masterLoadFile(mstData)
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
