import _ from 'lodash'
import React, { PureComponent } from 'react'
import {
  ListGroupItem, ListGroup, Button,
} from 'react-bootstrap'
import FontAwesome from 'react-fontawesome'
import { connect } from 'react-redux'
import { remote } from 'electron'
import {shipImgType, getShipImgPath} from '../../../game-misc'

import { PTyp } from '../../../ptyp'

const downloadUrl =
  remote.getCurrentWebContents().downloadURL

const imgList = _.flatMap(shipImgType, ty =>
  ty === 'album_status' ?
    [{ty, damaged: false}] :
    [{ty, damaged: false}, {ty, damaged: true}]
)

class GalleryViewP2Impl extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    style: PTyp.object.isRequired,
    // connected:
    serverIp: PTyp.string.isRequired,
  }

  render() {
    const {mstId, serverIp, style} = this.props
    return (
      <ListGroup style={style}>
        {
          imgList.map(x => {
            const url = `http://${serverIp}${getShipImgPath(mstId, x.ty, x.damaged)}`
            return (
              <ListGroupItem
                key={`${mstId},${x.ty},${x.damaged}`}
                style={{
                  textAlign: 'center',
                }}
              >
                <img
                  style={{maxWidth: '100%', height: 'auto'}}
                  src={url}
                  alt={`ship=${mstId}, type=${x.ty}, damaged=${x.damaged}`}
                />
                <div style={{display: 'flex', flexDirection: 'row-reverse'}}>
                  <Button
                    onClick={() => downloadUrl(url)}
                    bsSize="small"
                  >
                    <FontAwesome name="save" />
                  </Button>
                </div>
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
