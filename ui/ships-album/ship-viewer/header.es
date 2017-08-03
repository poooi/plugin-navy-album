import _ from 'lodash'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { mergeMapStateToProps } from 'subtender'
import { createStructuredSelector } from 'reselect'

import { headerInfoSelector } from './selectors'
import {
  shipGraphSourcesSelector,
} from '../selectors'

import { PTyp } from '../../../ptyp'

class HeaderImpl extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipName: PTyp.string.isRequired,
    typeName: PTyp.string.isRequired,
    shipGraphSources: PTyp.object.isRequired,
  }
  render() {
    const {mstId, shipName, typeName, shipGraphSources} = this.props
    const headerSource = _.get(shipGraphSources,1)
    return (
      <div style={{display: 'flex', alignItems: 'center', height: 40}}>
        <div style={{display: 'flex', alignItems: 'baseline', flex: 1}}>
          <div
            style={{
              fontSize: '1.2em',
              alignSelf: 'flex-start',
            }}
          >
            {typeName}
          </div>
          <div
            style={{
              marginLeft: '.4em',
              fontSize: '1.8em',
              fontWeight: 'bold',
            }}>
            {shipName}
          </div>
          <div
            style={{
              marginLeft: '.6em',
              fontSize: '1em',
            }}>
            ({mstId})
          </div>
        </div>
        <img
          style={{
            width: 160,
            height: 40,
            ...(headerSource ? {} : {display: 'none'}),
          }}
          alt={`mstId: ${mstId}`}
          src={headerSource}
        />
      </div>
    )
  }
}

const Header = connect(
  mergeMapStateToProps(
    headerInfoSelector,
    createStructuredSelector({
      shipGraphSources: shipGraphSourcesSelector,
    }))
)(HeaderImpl)

export { Header }
