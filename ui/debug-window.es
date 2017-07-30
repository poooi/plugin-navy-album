/* TODO remove disables after done */
/* eslint-disable no-console */
/* eslint-disable react/no-array-index-key */
/* eslint-disable react/prop-types */
import React, { Component } from 'react'
import { ListGroup, ListGroupItem } from 'react-bootstrap'
// import { remote } from 'electron'

import { readFromBufferP, codeToTag, extractImages } from 'swf-extract'

// const {openFocusedWindowDevTools} = remote.require('./lib/window')
// openFocusedWindowDevTools()

const initiateFetch = async setState => {
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
    setState(xs => ({
      ...xs,
      msgList: [...xs.msgList, imgData],
    }))
  })
}

class DebugWindow extends Component {
  constructor(props) {
    super(props)
    this.state = {
      msgList: [],
    }
  }

  componentDidMount() {
    initiateFetch(this.setState.bind(this))
  }

  render() {
    return (
      <div className="navy-album-main">
        <ListGroup>
          {
            this.state.msgList.map((msg,ind) =>
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

export { DebugWindow }
