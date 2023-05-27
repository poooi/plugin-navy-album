import { createStructuredSelector } from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { modifyObject } from 'subtender'

import Slider from '../../../assets/rc-slider.min'

import { PTyp } from '../../../ptyp'
import { mapDispatchToProps } from '../../../store'
import { levelSelector } from '../selectors'
import { minLevelSelector } from './selectors'

class LevelSliderImpl extends PureComponent {
  static propTypes = {
    level: PTyp.number.isRequired,
    minLevel: PTyp.number.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  handleChangeLevel = level =>
    this.props.uiModify(
      modifyObject(
        'shipsAlbum',
        modifyObject(
          'shipViewer',
          modifyObject(
            'level', () => level
          )
        )
      )
    )

  render() {
    const {level,minLevel} = this.props
    return (
      <div
        style={{
          display: 'flex',
          alignItems: 'baseline',
          width: '100%',
          height: '4em',
          marginBottom: '.4em',
          overflowX: 'hidden',
        }}>
        <span
          style={{
            marginLeft: '1em',
            fontSize: '1.8em', width: '3.6em',
          }}>
          Lv. {level}
        </span>
        <Slider
          style={{
            flex: 1,
            margin: '.3em 2em',
          }}
          value={level}
          onChange={this.handleChangeLevel}
          min={minLevel}
          max={180}
          marks={{
            [minLevel]: `Lv. ${minLevel}`,
            99: 'Lv. 99',
            180: 'Lv. 180',
          }}
        />
      </div>
    )
  }
}

const LevelSlider = connect(
  createStructuredSelector({
    level: levelSelector,
    minLevel: minLevelSelector,
  }),
  mapDispatchToProps
)(LevelSliderImpl)

export { LevelSlider }
