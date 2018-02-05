// import _ from 'lodash'
import React from 'react'
import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'
import { join } from 'path-extra'
import { readJsonSync } from 'fs-extra'
import { store, extendReducer } from 'views/create-store'

import {
  reducer,
  withBoundActionCreator,
  boundActionCreators,
  initState,
} from '../store'
import { NavyAlbum } from './navy-album'
import { loadPState } from '../p-state'
import { globalSubscribe } from '../observers'
import { loadSwfCache } from '../swf-cache'
import { loadMasterDataFile } from '../store/ext-root/master'

const {$} = window

$('#fontawesome-css')
  .setAttribute('href', require.resolve('font-awesome/css/font-awesome.css'))

extendReducer('poi-plugin-navy-album', reducer)

globalSubscribe()

/*
   TODO

   - collapsible type group?
   - needs some refactoring
   - needs to move around some selectors
   - ship album
       - handle special CGs
   - game update:
       - preview CGs in tooltip

 */

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
        readJsonSync(join(__dirname,'..','assets','default-digest.json'))
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

ReactDOM.render(
  <Provider store={store}>
    <div
      style={{margin: "0 1%", minWidth: 600}}
      className="navy-album-main">
      <NavyAlbum />
    </div>
  </Provider>,
  $("#content-root")
)
