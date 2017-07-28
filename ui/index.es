import React, { Component } from 'react'
import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'
import { store, extendReducer } from 'views/create-store'

const {$} = window

window.store = store

$('#fontawesome-css')
  .setAttribute('href', require.resolve('font-awesome/css/font-awesome.css'))

class NavyAlbumMain extends Component {
  render() {
    return (
      <div className="navy-album-main">
        TODO
      </div>
    )
  }
}

// extendReducer('poi-plugin-navy-album', reducer)

ReactDOM.render(
  <Provider store={store}>
    <NavyAlbumMain />
  </Provider>,
  $("#content-root"))
