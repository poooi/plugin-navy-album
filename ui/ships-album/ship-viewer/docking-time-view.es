import _ from 'lodash'
import { modifyObject } from 'subtender'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import Slider from '../../../assets/rc-slider.min'

import { PTyp } from '../../../ptyp'
import { mapDispatchToProps } from '../../../store'
import {
  shipViewerSelector,
} from '../selectors'
import {
  getDockingFactorFuncSelector,
} from '../../../selectors'
import {
  computeDockingTimePerHpWith,
  computeHealthState,
  healthStateColors,
} from '../../../game-misc'

/* pretty-printing a time
   - MM:SS is always shown
   - h:MM:SS where it's not necessary to pad "h"
 */
const pprTime = timeInSec => {
  const secondPart = timeInSec % 60
  const minute = Math.floor(timeInSec/60)
  const minutePart = minute % 60
  const hourPart = Math.floor(minute/60)
  const secondText = _.padStart(secondPart,2,'0')
  const minuteText = _.padStart(minutePart,2,'0')
  if (hourPart > 0) {
    return `${hourPart}:${minuteText}:${secondText}`
  } else {
    return `${minuteText}:${secondText}`
  }
}

@connect(
  (state, {mstId}) => {
    const stype = _.get(state.const,['$ships',mstId,'api_stype'])
    const nowHp = shipViewerSelector(state).dockingCurrentHp
    const getDockingFactor = getDockingFactorFuncSelector(state)
    const computeDockingTimePerHp =
      computeDockingTimePerHpWith(getDockingFactor)
    return {stype, nowHp, computeDockingTimePerHp}
  },
  mapDispatchToProps
)
class DockingTimeView extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    stype: PTyp.number.isRequired,
    level: PTyp.number.isRequired,
    nowHp: PTyp.number.isRequired,
    maxHp: PTyp.number.isRequired,
    uiModify: PTyp.func.isRequired,

    // connected:
    computeDockingTimePerHp: PTyp.func.isRequired,
  }

  componentDidMount() {
    this.checkInconsistentNowHp(this.props.nowHp, this.props.maxHp)
  }

  componentWillReceiveProps(nextProps) {
    this.checkInconsistentNowHp(nextProps.nowHp, nextProps.maxHp)
  }

  modifyNowHp = newNowHp =>
    this.props.uiModify(
      modifyObject(
        'shipsAlbum',
        modifyObject(
          'shipViewer',
          modifyObject(
            'dockingCurrentHp', () => newNowHp
          )
        )
      )
    )

  // detect cases where nowHp > maxHp and try to correct it
  checkInconsistentNowHp = (nowHp, maxHp) => {
    if (!_.isInteger(maxHp) || maxHp <= 1)
      return
    if (nowHp > maxHp) {
      this.modifyNowHp(1)
    }
  }

  render() {
    const {nowHp, maxHp, computeDockingTimePerHp} = this.props
    if (!_.isInteger(maxHp) || maxHp <= 1 || nowHp > maxHp) {
      return (<div style={{display: 'none'}} />)
    }
    const {style, stype, level} = this.props
    const timePerHp = computeDockingTimePerHp(stype,level)
    const timeInSec = nowHp === maxHp ? 0 :
      Math.floor(30 + timePerHp * (maxHp - nowHp))
    return (
      <div
        style={{
          display: 'flex',
          ...style,
        }}
      >
        <div style={{width: '7em'}}>Docking Time</div>
        <div
          style={{
            width: '5em',
            fontSize: '120%',
            fontWeight: 'bold',
          }}>
          {pprTime(timeInSec)}
        </div>
        <div
          style={{
            width: '4.5em',
            fontSize: '120%',
            fontWeight: 'bold',
            color: healthStateColors[computeHealthState(nowHp,maxHp)],
          }}>
          {`${nowHp}/${maxHp}`}
        </div>
        <Slider
          style={{
            flex: 1,
            margin: '.3em 2em',
          }}
          value={nowHp}
          onChange={this.modifyNowHp}
          min={1}
          max={maxHp}
        />
      </div>
    )
  }
}

export {
  DockingTimeView,
}
