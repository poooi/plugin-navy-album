import _ from 'lodash'
import React, { Component } from 'react'
import ReactDOM from 'react-dom'
import { DropdownButton, MenuItem, ListGroup, ListGroupItem } from 'react-bootstrap'
import { connect, Provider } from 'react-redux'
import { createStructuredSelector } from 'reselect'
import { store, extendReducer } from 'views/create-store'
import { readFromBufferP, extractImages } from 'swf-extract'

import { reducer } from '../store'
import {shipGraphInfoSelector} from '../selectors'


const {$} = window
$('#fontawesome-css')
  .setAttribute('href', require.resolve('font-awesome/css/font-awesome.css'))

const SERVERS = [
  '203.104.209.71',
  '203.104.209.87',
  '125.6.184.16',
  '125.6.187.205',
  '125.6.187.229',
  '125.6.187.253',
  '125.6.188.25',
  '203.104.248.135',
  '125.6.189.7',
  '125.6.189.39',
  '125.6.189.71',
  '125.6.189.103',
  '125.6.189.135',
  '125.6.189.167',
  '125.6.189.215',
  '125.6.189.247',
  '203.104.209.23',
  '203.104.209.39',
  '203.104.209.55',
  '203.104.209.102',
]

const initiateFetch = async (shipInfo, token, setState) => {
  const server = _.sample(SERVERS)
  const {graphInfo} = shipInfo
  const {fileName, versionStr} = graphInfo
  const fetched =
    await fetch(`http://${server}/kcs/resources/swf/ships/${fileName}.swf?VERSION=${versionStr}`)
  if (! fetched.ok)
    return console.error('fetch failed.')
  const ab = await fetched.arrayBuffer()

  const swfData = await readFromBufferP(new Buffer(ab))
  extractImages(swfData.tags).map(p => p.then(imgData => {
    setState(state => {
      if (state.token !== token)
        return state
      return {
        ...state,
        images: [...state.images, imgData],
      }
    })
  }))
}

class NavyAlbumMainImpl extends Component {
  constructor(props) {
    super(props)
    this.state = {
      images: [],
      mstId: null,
      token: null,
      shipName: '',
    }
  }

  handleSelect = info => {
    const token = Number(new Date())
    const {mstId, shipName} = info

    this.setState({mstId, shipName, token, images: []}, () =>
      initiateFetch(info, token, this.setState.bind(this)))
  }

  render() {
    const mstIdPrefix = this.state.mstId ? `${this.state.mstId}-` : 'none-'
    const dspShipName = this.state.mstId ? `${this.state.shipName} (${this.state.mstId})` : 'N/A'
    return (
      <div className="navy-album-main">
        <DropdownButton
          title={dspShipName}
          onSelect={this.handleSelect}
          id="na-dropdown">
          {
            this.props.shipGraphInfo.map(info => (
              <MenuItem eventKey={info} key={info.mstId}>
                {`${info.shipName} (${info.mstId})`}
              </MenuItem>
            ))
          }
        </DropdownButton>
        <ListGroup>
          {
            this.state.images.map(msg =>
              msg && (
                <ListGroupItem key={`${mstIdPrefix}-${msg.characterId}`}>
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
  createStructuredSelector({
    shipGraphInfo: shipGraphInfoSelector,
  }),
  null
)(NavyAlbumMainImpl)

extendReducer('poi-plugin-navy-album', reducer)

ReactDOM.render(
  <Provider store={store}>
    <NavyAlbumMain />
  </Provider>,
  $("#content-root"))
