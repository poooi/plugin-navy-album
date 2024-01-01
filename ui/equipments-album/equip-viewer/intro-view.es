import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { mergeMapStateToProps } from 'subtender'

import {
  equipRawInfoSelector,
} from '../selectors'
import {
  serverIpSelector,
} from '../../../selectors'
import {
  isAbyssalShipMstId,
  getEquipImgPath,
} from '../../../game-misc'

import { PTyp } from '../../../ptyp'

const id = x => x

@connect(
  mergeMapStateToProps(
    equipRawInfoSelector,
    state => ({serverIp: serverIpSelector(state)}),
  )
)
class IntroView extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    $equip: PTyp.object.isRequired,
    serverIp: PTyp.string.isRequired,
  }

  render() {
    const {style, $equip, serverIp} = this.props
    const mstId = $equip.api_id
    if (isAbyssalShipMstId(mstId)) {
      return <div style={{display: 'none'}} />
    }
    const prefix = `http://${serverIp}`

    return (
      <div style={style}>
        <div
          style={{
            display: 'flex',
            alignItems: 'center',
          }}>
          <img
            key={`card-${mstId}`}
            src={`${prefix}${getEquipImgPath(mstId, 'card')}`}
            alt="equip-img"
            style={{width: 260, height: 260}}
          />
          <div style={{
            flex: 1,
            marginLeft: 10,
            fontSize: '1.2em',
          }}>
            {
              (
                /* TODO: wtf? */
                false &&
                $equip.api_info.split('<br>').map((text,ind) => (
                  <p key={id(ind)}>
                    {text}
                  </p>
                ))
              )
            }
          </div>
        </div>
        <div
          style={{
            display: 'flex',
            justifyContent: 'space-around',
          }}
        >
          {
            [
              'item_character',
              'item_up',
              'item_on',
            ].map((which, ind) => (
              <img
                style={{height: 200, width: 'auto'}}
                key={`${which}-${id(ind)}`}
                alt={`eqp-extra-${ind}`}
                src={`${prefix}${getEquipImgPath(mstId, which)}`}
              />
            ))
          }
        </div>
      </div>
    )
  }
}

export { IntroView }
