import _ from 'lodash'
import { readJsonSync } from 'fs-extra'
import { join } from 'path-extra'

import React, { PureComponent } from 'react'

import { PTyp } from '../../../ptyp'
import { EquipmentsView } from './equipments-view'

import { StatsView } from './stats-view'

const abyssalInfo = readJsonSync(
  join(__dirname,'..', '..','..','assets','abyssal.json')
)

const mkStats = (_s, $abyssal) => {
  const st = path => {
    const v = _.get($abyssal,path)
    return _.isInteger(v) ? String(v) : '???'
  }

  return {
    hp: st('HP'),
    fire: st('FP'),
    armor: st('AR'),
    torpedo: st('TP'),
    evasion: st('EV'),
    antiair: st('AA'),
    cap: _.sum($abyssal.SLOTS.filter(x => _.isInteger(x) && x > 0)),
    asw: st('ASW'),
    speed: st('SPD'),
    los: st('LOS'),
    range: st('RNG'),
    luck: st('LUK'),
  }
}

class AbyssalInfoView extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipGraphSource: PTyp.string.isRequired,
    $ship: PTyp.object.isRequired,
  }
  render() {
    const {mstId, shipGraphSource, $ship} = this.props
    const $abyssal = abyssalInfo[mstId]
    const {__} = window
    const hasAbyssalEquipsInfo =
      $abyssal &&
      Array.isArray($abyssal.EQUIPS) &&
      Array.isArray($abyssal.SLOTS)
    return (
      <div style={{margin: '.2em', display: 'flex', flexDirection: 'column', alignItems: 'center'}}>
        <img
          style={{maxWidth: '100%', height: 'auto'}}
          src={shipGraphSource}
          alt={__('ShipsTab.WaitingDataFor',mstId)}
        />
        <div style={{
          display: 'flex',
          marginTop: '1.2em',
          width: '100%',
        }}>
          {
            hasAbyssalEquipsInfo && (
              <div style={{
                flex: 3,
                display: 'flex',
                justifyContent: 'space-around',
              }}>
                <EquipmentsView
                  style={{
                    width: '100%',
                    maxWidth: 300,
                  }}
                  slotCount={$ship.api_slot_num}
                  equips={
                    _.zip($abyssal.SLOTS, $abyssal.EQUIPS).map(([cap, eMstId]) =>
                      ({cap, mstId: eMstId}))
                  }
                />
              </div>
            )
          }
          {
            $abyssal && (
              <div style={{
                flex: 5,
                marginLeft: '.8em',
                display: 'flex',
                justifyContent: 'space-around',
              }}>
                <StatsView
                  stats={mkStats($ship, $abyssal)}
                  style={{
                    width: '100%',
                    maxWidth: 350,
                  }}
                />
              </div>
            )
          }
        </div>
      </div>
    )
  }
}

export { AbyssalInfoView }
