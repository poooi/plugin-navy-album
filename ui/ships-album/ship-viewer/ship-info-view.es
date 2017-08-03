import _ from 'lodash'
import React, { PureComponent } from 'react'

import { PTyp } from '../../../ptyp'
import { ships as wctfShips } from '../../../wctf'

import { EquipmentsView } from './equipments-view'

class ShipInfoView extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipGraphSource: PTyp.string.isRequired,
    $ship: PTyp.object.isRequired,
  }

  render() {
    const {mstId, shipGraphSource, $ship} = this.props
    const wctfShip = wctfShips[mstId]
    const equipIds = _.get(wctfShip,'equip',[])
    const equips = _.zip($ship.api_maxeq, equipIds).map(([slotNum,mstIdRaw]) =>
      ({cap: slotNum, mstId: _.isInteger(mstIdRaw) ? mstIdRaw : null}))
    return (
      <div style={{display: 'flex', margin: '.2em'}}>
        <img
          style={{width: 218, height: 300}}
          src={shipGraphSource}
          alt={`Data not yet available for ${mstId}`}
        />
        <div style={{flex: 1, display: 'flex', justifyContent: 'space-around'}}>
          <EquipmentsView
            slotCount={$ship.api_slot_num}
            equips={equips}
            style={{
              marginLeft: '.4em',
              flex: 1,
              maxWidth: 270,
            }}
          />
        </div>
      </div>
    )
  }
}

export { ShipInfoView }
