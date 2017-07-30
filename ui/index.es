import React from 'react'
import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'
import { store, extendReducer } from 'views/create-store'

import { reducer } from '../store'
import { NavyAlbumMain } from './navy-album-main'

const {$} = window
$('#fontawesome-css')
  .setAttribute('href', require.resolve('font-awesome/css/font-awesome.css'))

extendReducer('poi-plugin-navy-album', reducer)

ReactDOM.render(
  <Provider store={store}>
    <NavyAlbumMain />
  </Provider>,
  $("#content-root"))
