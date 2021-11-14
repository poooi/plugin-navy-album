import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { mergeMapStateToProps } from 'subtender'

import {
  equipRawInfoSelector,
} from '../selectors'
import {
  serverIpSelector,
} from '../../../selectors'

import { PTyp } from '../../../ptyp'

class IntroViewImpl extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    $equip: PTyp.object.isRequired,
    serverIp: PTyp.string.isRequired,
  }

  render() {
    const {style, $equip, serverIp} = this.props
    const mstId = $equip.api_id
    if (mstId > 500) {
      return <div style={{display: 'none'}} />
    }
    const id = x => x
    const mstIdStr = String(mstId).padStart(3,'0')
    /*
      TODO: it seems that new source uses `resources/slot/` that we'll need to migrate to
      at some point in future.

      Reference: SlotLoader.getPath of main.js
     */
    const prefix = `http://${serverIp}/kcs/resources/image/slotitem/`

    return (
      <div style={style}>
        <div
          style={{
            display: 'flex',
            alignItems: 'center',
          }}>
          <img
            key={`card-${mstId}`}
            src={`${prefix}card/${mstIdStr}.png`}
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
              `item_character/${mstIdStr}.png`,
              `item_up/${mstIdStr}.png`,
              `item_on/${mstIdStr}.png`,
            ].map((src, ind) => (
              <img
                style={{height: 200, width: 'auto'}}
                key={src}
                alt={`eqp-extra-${ind}`}
                src={`${prefix}${src}`}
              />
            ))
          }
        </div>
      </div>
    )
  }
}

const IntroView = connect(
  mergeMapStateToProps(
    equipRawInfoSelector,
    state => ({serverIp: serverIpSelector(state)}),
  )
)(IntroViewImpl)

export { IntroView }
