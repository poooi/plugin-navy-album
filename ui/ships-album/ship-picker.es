import React, { Component } from 'react'
import { connect } from 'react-redux'
import {
  ListGroup, ListGroupItem,
} from 'react-bootstrap'

import { shipsInfoSelectorForView } from './selectors'
import { PTyp } from '../../ptyp'

class ShipPickerImpl extends Component {
  static propTypes = {
    groupped: PTyp.bool.isRequired,
    wrappedShipsInfo: PTyp.array.isRequired,
  }

  render() {
    const {groupped, wrappedShipsInfo} = this.props
    return (
      <ListGroup>
        {
          wrappedShipsInfo.map(wrapped => {
            let key
            let content
            let needPadding
            if (wrapped.type === 'stype') {
              const {typeName, stype} = wrapped
              key = `stype-${stype}`
              content = `${typeName} (${stype})`
              needPadding = false
            } else {
              const {mstId, name} = wrapped.info
              key = `mstId-${wrapped.info.mstId}`
              content = `${name} (${mstId})`
              needPadding = groupped
            }
            return (
              <ListGroupItem key={key} style={{padding: 0}}>
                <div style={needPadding ? {paddingLeft: '2em'} : {}}>
                  {content}
                </div>
              </ListGroupItem>
            )
          })
        }
      </ListGroup>
    )
  }
}

const ShipPicker = connect(
  shipsInfoSelectorForView,
)(ShipPickerImpl)

export { ShipPicker }
