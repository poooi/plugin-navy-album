import React, { PureComponent } from 'react'
import { connect } from 'react-redux'

import {
  equipRawInfoSelector,
} from '../selectors'

import { PTyp } from '../../../ptyp'

class IntroViewImpl extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    $equip: PTyp.object.isRequired,
  }

  render() {
    const {style, $equip} = this.props
    const mstId = $equip.api_id
    if (mstId > 500) {
      return <div style={{display: 'none'}} />
    }
    const id = x => x
    const {serverIp} = window
    const mstIdStr = String(mstId).padStart(3,'0')
    const prefix = `http://${serverIp}/kcs/resources/image/slotitem/`

    /* [
     *   [`card/${mstIdStr}.png`, {width: 260, height: 260}],
     *   [`item_character/${mstIdStr}.png`, {width: 'auto', height: 260}],
     *   [`item_up/${mstIdStr}.png`, {width: 'auto', height: 260}],
     *   [`item_on/${mstIdStr}.png`, {width: 'auto', height: 260}],
     * ].map(([src,sty], ind) => (
     */

    return (
      <div style={style}>
        <div
          style={{
            display: 'flex',
            alignItems: 'center',
          }}>
          <img
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
              $equip.api_info.split('<br>').map((text,ind) => (
                <p key={id(ind)}>
                  {text}
                </p>
              ))
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
                key={id(ind)}
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

const IntroView = connect(equipRawInfoSelector)(IntroViewImpl)

export { IntroView }
