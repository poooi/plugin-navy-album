import _ from 'lodash'
import { readJsonSync } from 'fs-extra'
import { join } from 'path-extra'
import { createStructuredSelector } from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'

import { PTyp } from '../../../ptyp'
import { EquipmentsView } from './equipments-view'
import {
  getShipImgSrcFuncSelector,
} from '../../../selectors'
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

@connect(
  createStructuredSelector({
    getShipImgSrc: getShipImgSrcFuncSelector,
  })
)
class AbyssalInfoView extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    $ship: PTyp.object.isRequired,
    debuffFlag: PTyp.bool.isRequired,

    // connected
    getShipImgSrc: PTyp.func.isRequired,
  }

  render() {
    const {
      mstId, $ship, debuffFlag,
      getShipImgSrc,
    } = this.props
    const $abyssal = abyssalInfo[mstId]
    const {__} = window.i18n["poi-plugin-navy-album"]
    const hasAbyssalEquipsInfo =
      $abyssal &&
      Array.isArray($abyssal.EQUIPS) &&
      Array.isArray($abyssal.SLOTS)

    return (
      <div style={{margin: '.2em', display: 'flex', flexDirection: 'column', alignItems: 'center'}}>
        <img
          style={{maxWidth: '100%', height: 'auto'}}
          src={getShipImgSrc(mstId, 'full', false, debuffFlag)}
          alt={__('ShipsTab.WaitingDataFor',mstId)}
          key={`${mstId},${debuffFlag}`}
        />
        <div style={{
          display: 'flex',
          marginTop: '1.2em',
          width: '100%',
        }}>
          {
            (hasAbyssalEquipsInfo && false) && (
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
                  prefix=""
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
