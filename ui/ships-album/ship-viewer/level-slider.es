import { createStructuredSelector } from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import Slider from 'rc-slider'
import { modifyObject } from 'subtender'

import { PTyp } from '../../../ptyp'
import { mapDispatchToProps } from '../../../store'
import { levelSelector } from '../selectors'

class LevelSliderImpl extends PureComponent {
  static propTypes = {
    level: PTyp.number.isRequired,
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
    const {level} = this.props
    return (
      <div
        style={{
          display: 'flex',
          alignItems: 'baseline',
          width: '100%',
          height: '4em',
          marginBottom: '1.8em',
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
          min={1 /* TODO minimum possible level */}
          max={165}
          marks={{
            // TODO
            1: 'Lv. 1',
            99: 'Lv. 99',
            165: 'Lv. 165',
          }}
        />
      </div>
    )
  }
}

const LevelSlider = connect(
  createStructuredSelector({
    level: levelSelector,
  }),
  mapDispatchToProps
)(LevelSliderImpl)

export { LevelSlider }
