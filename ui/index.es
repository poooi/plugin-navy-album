import _ from 'lodash'
import React from 'react'
import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'
import { store, extendReducer } from 'views/create-store'

import { reducer, withBoundActionCreator, initState } from '../store'
import { NavyAlbum } from './navy-album'
import { loadPState } from '../p-state'
import { readIndexFile } from '../swf-cache'

const {$} = window

$('#fontawesome-css')
  .setAttribute('href', require.resolve('font-awesome/css/font-awesome.css'))
$('#rc-slider-css')
  .setAttribute('href', require.resolve('rc-slider/assets/index.css'))

extendReducer('poi-plugin-navy-album', reducer)

// start loading p-state
setTimeout(() => {
  let newUiState = {}
  try {
    const pState = loadPState()
    if (pState !== null)
      newUiState = pState.ui
  } catch (e) {
    console.error('error while initializing', e)
  } finally {
    withBoundActionCreator(
      ({uiReady}) => uiReady(newUiState)
    )
  }
})

/*
   load swf cache index synchronously - this needs to be done before
   observers are initialized, otherwise first ship graph request will
   always end up missing the cache.
*/
{
  let newDiskFiles = initState.swfDatabase.diskFiles
  try {
    const indexContent = readIndexFile()
    if (!_.isEmpty(indexContent.files))
      newDiskFiles = indexContent.files
  } catch (e) {
    console.error('error while initializing', e)
  } finally {
    withBoundActionCreator(
      ({swfDatabaseDiskFilesReady}) =>
        swfDatabaseDiskFilesReady(newDiskFiles)
    )
  }
}

ReactDOM.render(
  <Provider store={store}>
    <div
      style={{margin: "0 1%", minWidth: 600}}
      className="navy-album-main">
      <NavyAlbum />
    </div>
  </Provider>,
  $("#content-root"))
