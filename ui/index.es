import React from 'react'
import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'
import { store, extendReducer } from 'views/create-store'

import { reducer, withBoundActionCreator } from '../store'
import { NavyAlbum } from './navy-album'
import { loadPState } from '../p-state'

const {$} = window

$('#fontawesome-css')
  .setAttribute('href', require.resolve('font-awesome/css/font-awesome.css'))
$('#rc-slider-css')
  .setAttribute('href', require.resolve('rc-slider/assets/index.css'))

extendReducer('poi-plugin-navy-album', reducer)

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

ReactDOM.render(
  <Provider store={store}>
    <div
      style={{margin: "0 1%", minWidth: 600}}
      className="navy-album-main">
      <NavyAlbum />
    </div>
  </Provider>,
  $("#content-root"))
