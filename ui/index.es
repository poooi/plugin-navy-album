import _ from 'lodash'
import React from 'react'
import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'
import { join } from 'path-extra'
import { readJsonSync } from 'fs-extra'
import { store, extendReducer } from 'views/create-store'

import { reducer, withBoundActionCreator, initState } from '../store'
import { NavyAlbum } from './navy-album'
import { loadPState } from '../p-state'
import { readIndexFile } from '../swf-cache'
import { globalSubscribe } from '../observers'

const {$} = window

$('#fontawesome-css')
  .setAttribute('href', require.resolve('font-awesome/css/font-awesome.css'))
$('#rc-slider-css')
  .setAttribute('href', require.resolve('rc-slider/assets/index.css'))

extendReducer('poi-plugin-navy-album', reducer)

globalSubscribe()

/*
   TODO

   - rework remodel panel
   - rc-slider dist
   - reduce asset size
   - collapsible type group?
   - needs some refactoring
   - needs to move around some selectors

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
  }

  // load swf cache index asynchronously
  let newDiskFiles = initState.swfDatabase.diskFiles
  try {
    const indexContent = readIndexFile()
    if (!_.isEmpty(_.get(indexContent,'files')))
      newDiskFiles = indexContent.files
  } catch (e) {
    console.error('error while initializing', e)
  } finally {
    withBoundActionCreator(
      ({swfDatabaseDiskFilesReady}) =>
        swfDatabaseDiskFilesReady(newDiskFiles)
    )
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
  $("#content-root"))
