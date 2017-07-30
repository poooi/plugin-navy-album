import React, { Component } from 'react'
import ReactDOM from 'react-dom'
import { connect, Provider } from 'react-redux'
import { store, extendReducer } from 'views/create-store'
import { reducer } from '../store'
import { DebugWindow } from './debug-window'

const {$} = window
$('#fontawesome-css')
  .setAttribute('href', require.resolve('font-awesome/css/font-awesome.css'))

class NavyAlbumMainImpl extends Component {
  render() {
    return (
      <div className="navy-album-main">
        <DebugWindow />
      </div>
    )
  }
}

const NavyAlbumMain = connect(
  null,
  null
)(NavyAlbumMainImpl)

extendReducer('poi-plugin-navy-album', reducer)

ReactDOM.render(
  <Provider store={store}>
    <NavyAlbumMain />
  </Provider>,
  $("#content-root"))
