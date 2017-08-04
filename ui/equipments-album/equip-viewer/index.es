import { createStructuredSelector } from 'reselect'
import React, { Component } from 'react'
import { connect } from 'react-redux'
import {
  Panel,
} from 'react-bootstrap'

import {
  mstIdSelector,
} from '../selectors'

import { PTyp } from '../../../ptyp'
import { Header } from './header'
import { IntroView } from './intro-view'

class EquipViewerImpl extends Component {
  static propTypes = {
    style: PTyp.object.isRequired,
    mstId: PTyp.number.isRequired,
  }

  render() {
    const {style, mstId} = this.props
    return (
      <Panel
        className="equip-viewer"
        style={{
          ...style,
        }}
      >
        <div
          style={{flex: 1, height: 0, overflowY: 'auto'}}
        >
          <Header />
          {
            mstId < 501 && (
              <IntroView style={{}} />
            )
          }
          <div>StatsView</div>
        </div>
      </Panel>
    )
  }
}

const EquipViewer = connect(
  createStructuredSelector({
    mstId: mstIdSelector,
  })
)(EquipViewerImpl)

export { EquipViewer }
