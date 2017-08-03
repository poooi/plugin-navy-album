import _ from 'lodash'
import React, { PureComponent } from 'react'

import { PTyp } from '../../../ptyp'
import { ships as wctfShips } from '../../../wctf'

import { EquipmentsView } from './equipments-view'
import { StatsView } from './stats-view'
import { LevelSlider } from './level-slider'

const mkStats = ($ship, wctfShip) => {
  const ranged = propName => {
    const [x,y] = $ship[`api_${propName}`]
    return x === y ? `${x}` : `${x}~${y}`
  }

  // TODO: ev & los & asw are level-dependent
  const tmp = path => {
    const v = _.get(wctfShip, path)
    return _.isInteger(v) ? `${v} (at Lv. 99)` : '???'
  }

  return {
    hp: $ship.api_taik[0],
    fire: ranged('houg'),
    armor: ranged('souk'),
    torpedo: ranged('souk'),
    evasion: tmp('stat.evasion_max'),
    antiair: ranged('tyku'),
    cap: _.sum($ship.api_maxeq.filter(x => _.isInteger(x) && x > 0)),
    asw: tmp('stat.asw_max'),
    speed: String($ship.api_soku),
    los: tmp('stat.los_max'),
    range: String($ship.api_leng),
    luck: ranged('luck'),
  }
}

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
      <div style={{
        display: 'flex',
        flexDirection: 'column',
        margin: '.2em',
      }}>
        <div style={{
          width: '100%',
          display: 'flex',
          justifyContent: 'space-around',
        }}>
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
              stats={mkStats($ship, wctfShip)}
              style={{marginTop: '.8em'}}
            />
          </div>
        </div>
        <LevelSlider />
      </div>
    )
  }
}

export { ShipInfoView }
