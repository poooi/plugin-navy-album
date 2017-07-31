import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { headerInfoSelector } from './selectors'

import { PTyp } from '../../../ptyp'

class HeaderImpl extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipName: PTyp.string.isRequired,
    typeName: PTyp.string.isRequired,
  }
  render() {
    const {mstId, shipName, typeName} = this.props
    return (
      <div style={{display: 'flex', alignItems: 'baseline'}}>
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
    )
  }
}

const Header = connect(headerInfoSelector)(HeaderImpl)

export { Header }
