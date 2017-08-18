import React, { Component } from 'react'
import { mergeMapStateToProps } from 'subtender'
import { connect } from 'react-redux'
import {
  ListGroup, ListGroupItem,
  Panel,
  FormControl, Button,
} from 'react-bootstrap'
import FontAwesome from 'react-fontawesome'
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
    uiSwitchShip: PTyp.func.isRequired,
  }

  handleSelectMstId = mstId => () =>
    this.props.uiSwitchShip(mstId)

  render() {
    const {groupped, wrappedShipsInfo} = this.props
    return (
      <Panel
        className="ship-picker"
        style={{
          height: 0,
          flex: 1,
          marginBottom: 8,
        }}>
        <div style={{
          display: 'flex',
          alignItems: 'baseline',
          marginBottom: 8,
        }}>
          <FormControl type="text" placeholder="Search ..." readOnly />
          <Button bsSize="xsmall">
            <FontAwesome name="close" />
          </Button>
        </div>
        <ListGroup style={{
          flex: 1,
          overflowY: 'auto',
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
