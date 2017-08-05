import _ from 'lodash'
import { createStructuredSelector } from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'

import { PTyp } from '../../../ptyp'
import { ships as wctfShips } from '../../../wctf'
import {
  statsAtCurrentLevelSelector,
} from './selectors'
import { levelSelector } from '../selectors'

import { EquipmentsView } from './equipments-view'
import { StatsView } from './stats-view'
import { LevelSlider } from './level-slider'
import { ExtraInfoView } from './extra-info-view'
import { RemodelInfoView } from './remodel-info-view'

// TODO: hp is level-dependent, a bit complicated though.

const id = _.identity

const mkStats = ($ship, _wctfShip, statsL, level) => {
  const ranged = propName => {
    const [x,y] = $ship[`api_${propName}`]
    return x === y ? `${x}` : `${x}~${y}`
  }

  const mkLvlDpdStatView = value => {
    const text = _.isInteger(value) ? String(value) : '???'
    return (
      <div className={level === 99 ? '' : 'custom text-primary'}>
        {text}
      </div>
    )
  }

  return {
    hp: $ship.api_taik[0],
    fire: ranged('houg'),
    armor: ranged('souk'),
    torpedo: ranged('souk'),
    antiair: ranged('tyku'),
    cap: _.sum($ship.api_maxeq.filter(x => _.isInteger(x) && x > 0)),
    speed: String($ship.api_soku),
    range: String($ship.api_leng),
    luck: ranged('luck'),
    ..._.mapValues(statsL, mkLvlDpdStatView),
  }
}

const normalizeIntro = text =>
  _.compact(
    text.split('<br>').map(xs => xs.trim())
  )

class ShipInfoViewImpl extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipGraphSource: PTyp.string.isRequired,
    $ship: PTyp.object.isRequired,
    level: PTyp.number.isRequired,
    statsL: PTyp.object.isRequired,
  }

  render() {
    const {
      mstId, shipGraphSource, $ship,
      statsL, level,
    } = this.props
    const wctfShip = wctfShips[mstId]
    const equipIds = _.get(wctfShip,'equip',[])
    const equips = _.zip($ship.api_maxeq, equipIds).map(([slotNum,mstIdRaw]) =>
      ({cap: slotNum, mstId: _.isInteger(mstIdRaw) ? mstIdRaw : null}))

    const introMessaage = normalizeIntro($ship.api_getmes)

    return (
      <div
        className="ship-info-view"
        style={{
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
                maxWidth: 300,
              }}
            />
            <StatsView
              stats={mkStats($ship, wctfShip, statsL, level)}
              style={{
                marginTop: '.8em',
                maxWidth: 450,
                width: '100%',
              }}
            />
          </div>
        </div>
        {
          introMessaage && (
            <div style={{alignSelf: 'center'}}>
              {
                introMessaage.map((m,ind) => (
                  <p
                    style={{marginBottom: '.4em', fontSize: '1.2em'}}
                    key={id(ind)}>
                    {m}
                  </p>
                ))
              }
            </div>
          )
        }
        <LevelSlider />
        <div style={{
          width: '100%',
          display: 'flex',
          justifyContent: 'space-around',
        }}>
          <ExtraInfoView
            style={{maxWidth: 850, width: '100%'}}
            $ship={$ship}
            level={level}
          />
        </div>
        <div style={{
          width: '100%',
          display: 'flex',
        }}>
          <RemodelInfoView
            style={{width: '60%'}}
            mstId={mstId}
          />
        </div>
      </div>
    )
  }
}

const ShipInfoView = connect(
  createStructuredSelector({
    level: levelSelector,
    // Level-dependent stats
    statsL: statsAtCurrentLevelSelector,
  })
)(ShipInfoViewImpl)

export { ShipInfoView }
