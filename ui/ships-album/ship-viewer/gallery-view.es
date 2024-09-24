import _ from 'lodash'
import React, { PureComponent } from 'react'
import FontAwesome from 'react-fontawesome'
import { connect } from 'react-redux'
import { remote } from 'electron'

import {
  Button,
  Classes,
  Card,
} from '@blueprintjs/core'

import {
  shipImgType,
  isAbyssalShipMstId,
} from '../../../game-misc'
import {
  isMasterIdSpecialCGFuncSelector,
  getShipImgSrcFuncSelector,
} from '../../../selectors'
import {
  submarineTendersSelector,
} from './selectors'
import { PTyp } from '../../../ptyp'

const downloadUrl =
  remote.getCurrentWebContents().downloadURL

// main.js module: CutinSpSSF, look for `preload` or `_ready` function.
const battleShipSpecials = mstId =>
  [
    // Nelson: special
    571,
    // Nelson改: special
    576,

    // Rodney: special
    572,
    // Rodney改: special
    577,

    // 長門改二: special
    541,
    // 陸奥改二: special
    573,

    // Colorado: special
    601,
    // Colorado改: special
    1496,

    // Maryland: special
    913,
    // Maryland改: special
    918,
  ].indexOf(mstId) !== -1

// TODO: clean up
const mkImgListFriendly = (mstId, whales) => {
  const xs = _.flatMap(shipImgType, ty =>
    ty === 'album_status' ?
      [{ty, damaged: false}] :
      [{ty, damaged: false}, {ty, damaged: true}]
  )
  if (whales.includes(mstId)) {
    xs.push({ty: 'special', damaged: false})
    xs.push({ty: 'special', damaged: true})
  }
  if (battleShipSpecials(mstId)) {
    xs.push({ty: 'special', damaged: false})
  }
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

@connect(
  (state, props) => {
    // TODO: use selector
    const {mstId} = props
    const isMasterIdSpecialCG = isMasterIdSpecialCGFuncSelector(state)
    return {
      isSpecialCG: isMasterIdSpecialCG(mstId),
      getShipImgSrc: getShipImgSrcFuncSelector(state),
      whales: submarineTendersSelector(state),
    }
  }
)
class GalleryView extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    style: PTyp.object.isRequired,
    debuffFlag: PTyp.bool.isRequired,

    // connected:
    isSpecialCG: PTyp.bool.isRequired,
    getShipImgSrc: PTyp.func.isRequired,
    whales: PTyp.array.isRequired,
  }

  render() {
    const {
      mstId, style, debuffFlag,
      isSpecialCG, getShipImgSrc,
      whales,
    } = this.props
    const imgList = isSpecialCG ? imgListSpecialCG :
      isAbyssalShipMstId(mstId) ? imgListAbyssal : mkImgListFriendly(mstId, whales)

    return (
      <div
        style={style}
        className={Classes.LIST}
      >
        {
          imgList.map(x => {
            const src = getShipImgSrc(mstId, x.ty, x.damaged, debuffFlag)
            return (
              <Card
                key={`${mstId},${x.ty},${x.damaged}`}
                style={{
                  textAlign: 'center',
                }}
              >
                <img
                  style={{maxWidth: '100%', height: 'auto'}}
                  src={src}
                  alt={`ship=${mstId}, type=${x.ty}, damaged=${x.damaged}`}
                  key={`${mstId},${x.ty},${x.damaged},${debuffFlag}`}
                />
                <div style={{display: 'flex', flexDirection: 'row-reverse'}}>
                  <Button
                    style={{width: 27, height: 27}}
                    onClick={() => downloadUrl(src)}
                    small
                    text={<FontAwesome name="save" />}
                  />
                </div>
              </Card>
            )
          })
        }
      </div>
    )
  }
}

export { GalleryView }
