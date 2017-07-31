import React, { Component } from 'react'
import { mergeMapStateToProps, modifyObject } from 'subtender'
import { connect } from 'react-redux'
import {
  ListGroup, ListGroupItem,
  Panel,
} from 'react-bootstrap'

import {
  shipsInfoSelectorForView,
  listOptionsSelector,
  mstIdSelector,
} from './selectors'
import { PTyp } from '../../ptyp'
import { mapDispatchToProps } from '../../store'

class ShipPickerImpl extends Component {
  static propTypes = {
    groupped: PTyp.bool.isRequired,
    wrappedShipsInfo: PTyp.array.isRequired,
    expanded: PTyp.bool.isRequired,
    mstId: PTyp.number.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  handleSelectMstId = mstId => () =>
    this.props.uiModify(
      modifyObject(
        'shipsAlbum',
        modifyObject(
          'shipViewer',
          modifyObject(
            'mstId', () => mstId
          )
        )
      )
    )

  render() {
    const {groupped, wrappedShipsInfo, expanded} = this.props
    return (
      <Panel
        className="ship-picker"
        style={{
          height: expanded ? '86vh' : '89vh',
        }}>
        <ListGroup style={{
          height: '100%',
          overflowY: 'scroll',
        }}>
          {
            wrappedShipsInfo.map(wrapped => {
              let key
              let content
              let needPadding
              let onClick = null
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
                onClick = this.handleSelectMstId(mstId)
              }
              return (
                <ListGroupItem
                  key={key}
                  style={{padding: 0}}
                  onClick={onClick}
                  className="ship-picker-item">
                  <div style={{
                    paddingTop: '.4em',
                    paddingBottom: '.4em',
                    paddingLeft: '.4em',
                    ...(needPadding ? {paddingLeft: '2em'} : {}),
                  }}>
                    {content}
                  </div>
                </ListGroupItem>
              )
            })
          }
        </ListGroup>
      </Panel>
    )
  }
}

const ShipPicker = connect(
  mergeMapStateToProps(
    shipsInfoSelectorForView,
    listOptionsSelector,
    state => ({
      mstId: mstIdSelector(state),
    })
  ),
  mapDispatchToProps,
)(ShipPickerImpl)

export { ShipPicker }
