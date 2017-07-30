/* TODO remove disables after done */
/* eslint-disable no-console */
/* eslint-disable react/no-array-index-key */
/* eslint-disable react/prop-types */
import React, { Component } from 'react'
import ReactDOM from 'react-dom'
import { connect, Provider } from 'react-redux'
import { ListGroup, ListGroupItem } from 'react-bootstrap'
import { store, extendReducer } from 'views/create-store'
// import { remote } from 'electron'

import { readFromBufferP, codeToTag, extractImages } from 'swf-extract'
import { reducer, withBoundActionCreator } from '../store'
import { extSelector } from '../selectors'

const {$} = window

// const {openFocusedWindowDevTools} = remote.require('./lib/window')
// openFocusedWindowDevTools()

$('#fontawesome-css')
  .setAttribute('href', require.resolve('font-awesome/css/font-awesome.css'))

class NavyAlbumMainImpl extends Component {
  render() {
    return (
      <div className="navy-album-main">
        <ListGroup>
          {
            this.props.msgList.map((msg,ind) =>
              msg && (
                <ListGroupItem key={ind}>
                  {
                    (
                      (
                        typeof msg === 'object' &&
                        ['jpeg', 'png', 'gif'].includes(msg.imgType)) ? (
                          <div>
                            <div>
                              {
                                [
                                  `type: ${msg.imgType}`,
                                  `tag: ${codeToTag(msg.code)}`,
                                  `characterId: ${msg.characterId}`,
                                ].join(', ')
                              }
                            </div>
                            <img
                              src={`data:image/${msg.imgType};base64,${msg.imgData.toString('base64')}`}
                              alt="TEST"
                            />
                          </div>
                        ) : (
                          typeof msg !== 'object' ?
                            JSON.stringify(msg) :
                            JSON.stringify(Object.keys(msg))
                        )
                    )
                  }
                </ListGroupItem>
              )
            )
          }
        </ListGroup>
      </div>
    )
  }
}

const NavyAlbumMain = connect(
  state => {
    const msgList = extSelector(state)
    return {msgList}
  },
  null
)(NavyAlbumMainImpl);

(async () => {
  const fetched =
    await fetch('https://raw.githubusercontent.com/Javran/swf-extract/master/samples/sample1.swf')
    // await fetch('http://203.104.248.135/kcs/resources/swf/ships/bfsrsnqivpms.swf?VERSION=10')
    // await fetch('http://203.104.248.135/kcs/scenes/SallyMain.swf?version=3.2.2')
    // await fetch('http://203.104.248.135/kcs/resources/swf/commonAssets.swf?version=3.2.4')
  if (! fetched.ok)
    return console.error('fetch failed.')
  const ab = await fetched.arrayBuffer()

  const swfData = await readFromBufferP(new Buffer(ab))
  extractImages(swfData.tags).map(async p => {
    const imgData = await p
    withBoundActionCreator(({sendMsg}) => sendMsg(imgData))
  })
})()

extendReducer('poi-plugin-navy-album', reducer)

ReactDOM.render(
  <Provider store={store}>
    <NavyAlbumMain />
  </Provider>,
  $("#content-root"))
