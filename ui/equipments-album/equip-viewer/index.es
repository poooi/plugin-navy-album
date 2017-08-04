import { createStructuredSelector } from 'reselect'
import React, { Component } from 'react'
import { connect } from 'react-redux'
import {
  Panel,
} from 'react-bootstrap'

import { mstIdSelector } from '../selectors'

import { PTyp } from '../../../ptyp'
import { mapDispatchToProps } from '../../../store'

class EquipViewerImpl extends Component {
  static propTypes = {
    style: PTyp.object.isRequired,
    mstId: PTyp.number.isRequired,
  }

  render() {
    const {style, mstId} = this.props
    const id = x => x
    const {serverIp} = window
    const mstIdStr = String(mstId).padStart(3,'0')
    const prefix = `http://${serverIp}/kcs/resources/image/slotitem/`
    return (
      <Panel
        className="equip-viewer"
        style={style}
      >
        {
          mstId >= 500 ? (
            <div>Abyssal equipment</div>
          ) : (
            <div style={{display: 'flex', flexWrap: 'wrap', alignItems: 'center'}}>
              {
                [
                  `card/${mstIdStr}.png`,
                  `item_character/${mstIdStr}.png`,
                  `item_up/${mstIdStr}.png`,
                  `item_on/${mstIdStr}.png`,
                ].map((src, ind) => (
                  <img src={`${prefix}${src}`} style={{width: '20%', height: 'auto'}} alt={`img-${ind}`} key={id(ind)} />
                ))
              }
            </div>
          )
        }
      </Panel>
    )
  }
}

const EquipViewer = connect(
  createStructuredSelector({
    mstId: mstIdSelector,
  }),
  mapDispatchToProps,
)(EquipViewerImpl)

export { EquipViewer }
