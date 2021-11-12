import _ from 'lodash'
import React, { PureComponent } from 'react'
import {
  ListGroupItem, ListGroup, Button,
} from 'react-bootstrap'
import FontAwesome from 'react-fontawesome'
import { connect } from 'react-redux'
import { remote } from 'electron'
import {shipImgType, getShipImgPath} from '../../../game-misc'

import {
  isMasterIdSpecialCGFuncSelector,
} from '../../../selectors'

import { PTyp } from '../../../ptyp'

const downloadUrl =
  remote.getCurrentWebContents().downloadURL

const asWeCan = mstId => [184, 634, 635, 639, 640].indexOf(mstId) !== -1

// TODO: clean up
const mkImgListFriendly = mstId => {
  const xs = _.flatMap(shipImgType, ty =>
    ty === 'album_status' ?
      [{ty, damaged: false}] :
      [{ty, damaged: false}, {ty, damaged: true}]
  )
  if (asWeCan(mstId)) {
    xs.push({ty: 'special', damaged: false})
    xs.push({ty: 'special', damaged: true})
  }
  // TODO: big 7 and other special attacks.
  return xs
}

const imgListAbyssal = _.flatMap(['banner', 'full'], ty =>
  [{ty, damaged: false}]
)

const imgListSpecialCG = [
  {ty: 'card', damaged: false},
  {ty: 'character_up', damaged: false},
  {ty: 'character_up', damaged: true},
  {ty: 'character_full', damaged: false},
  {ty: 'character_full', damaged: true},
]

class GalleryViewImpl extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    style: PTyp.object.isRequired,
    debuffFlag: PTyp.bool.isRequired,

    // connected:
    serverIp: PTyp.string.isRequired,
    isSpecialCG: PTyp.bool.isRequired,
  }

  render() {
    const {mstId, serverIp, style, debuffFlag, isSpecialCG} = this.props
    const imgList = isSpecialCG ? imgListSpecialCG :
      mstId > 1500 ? imgListAbyssal : mkImgListFriendly(mstId)

    return (
      <ListGroup style={style}>
        {
          imgList.map(x => {
            const url = `http://${serverIp}${getShipImgPath(mstId, x.ty, x.damaged, debuffFlag)}`
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
                  key={`${mstId},${x.ty},${x.damaged},${debuffFlag}`}
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

const GalleryView = connect(
  (state, props) => {
    // TODO: use selector
    const serverIp = _.get(state, ['info', 'server', 'ip'])
    const {mstId} = props
    const isMasterIdSpecialCG = isMasterIdSpecialCGFuncSelector(state)
    return {
      serverIp,
      isSpecialCG: isMasterIdSpecialCG(mstId),
    }
  }
)(GalleryViewImpl)

export { GalleryView }
