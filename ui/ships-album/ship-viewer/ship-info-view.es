import _ from 'lodash'
import React, { PureComponent } from 'react'

import { PTyp } from '../../../ptyp'
import { ships as wctfShips } from '../../../wctf'

import { EquipmentsView } from './equipments-view'
import { StatsView } from './stats-view'

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
      <div style={{display: 'flex', margin: '.2em', justifyContent: 'space-around'}}>
        <div style={{flex: 3, display: 'flex', justifyContent: 'space-around'}}>
          <img
            style={{width: 218, height: 300}}
            src={shipGraphSource}
            alt={`Data not yet available for ${mstId}`}
          />
        </div>
        <div style={{
          flex: 5,
          marginLeft: '1em',
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
        }}>
          <EquipmentsView
            slotCount={$ship.api_slot_num}
            equips={equips}
            style={{
              width: '100%',
              maxWidth: 270,
            }}
          />
          <StatsView
            $ship={$ship}
            wctfShip={wctfShip}
            style={{marginTop: '.8em'}}
          />
        </div>
      </div>
    )
  }
}

export { ShipInfoView }
