import _ from 'lodash'
import React, { PureComponent } from 'react'
import {
  ListGroupItem, ListGroup, Button,
} from 'react-bootstrap'

import { connect } from 'react-redux'

import {shipImgType, getShipImgPath} from '../../../game-misc/p2-ship-img'

import { PTyp } from '../../../ptyp'

const imgList = _.flatMap(shipImgType, ty =>
  ty === 'album_status' ?
    [{ty, damaged: false}] :
    [{ty, damaged: false}, {ty, damaged: true}]
)

class GalleryViewP2Impl extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,

    // connected:
    serverIp: PTyp.string.isRequired,
  }

  render() {
    const {mstId, serverIp} = this.props
    return (
      <ListGroup>
        {
          imgList.map(x => {
            const url = `http://${serverIp}${getShipImgPath(mstId, x.ty, x.damaged)}`
            return (
              <ListGroupItem
                style={{
                  textAlign: 'center',
                }}
              >
                <img
                  style={{maxWidth: '100%', height: 'auto'}}
                  src={url}
                  alt={`ship=${mstId}, type=${x.ty}, damaged=${x.damaged}`}
                />
              </ListGroupItem>
            )
          })
        }
      </ListGroup>
    )
  }
}

const GalleryViewP2 = connect(
  state => {
    const serverIp = _.get(state, ['info', 'server', 'ip'])
    return {
      serverIp,
    }
  }
)(GalleryViewP2Impl)

export { GalleryViewP2 }
