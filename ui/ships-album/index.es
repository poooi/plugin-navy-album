import React, { Component } from 'react'
import { connect } from 'react-redux'
import {
  Button,
  ButtonGroup,
  ButtonToolbar,
  Panel,
} from 'react-bootstrap'
import {
  modifyObject,
  not,
} from 'subtender'

import { listOptionsSelector } from './selectors'
import { mapDispatchToProps } from '../../store'
import { PTyp } from '../../ptyp'

import { ShipPicker } from './ship-picker'
import { ShipViewer } from './ship-viewer'

class ShipsAlbumImpl extends Component {
  static propTypes = {
    expanded: PTyp.bool.isRequired,
    showSides: PTyp.object.isRequired,
    groupShipTypes: PTyp.bool.isRequired,
    groupRemodels: PTyp.bool.isRequired,

    uiModify: PTyp.func.isRequired,
  }

  modifyListOptions = modifier =>
    this.props.uiModify(
      modifyObject(
        'shipsAlbum',
        modifyObject(
          'listOptions', modifier
        )
      )
    )

  handleToggle = which => () =>
    this.modifyListOptions(
      modifyObject(which,not)
    )

  handleToggleSide = which => () =>
    this.modifyListOptions(
      modifyObject(
        'showSides',
        modifyObject(which,not)
      )
    )

  render() {
    const {
      expanded,
      showSides,
      groupShipTypes,
      groupRemodels,
    } = this.props
    const boolToBsStyle = v => v ? 'primary' : 'default'
    return (
      <div>
        <Panel
          style={{
            marginBottom: 8,
            ...(expanded ? {} : {display: 'none'}),
          }}>
          <ButtonToolbar>
            <ButtonGroup>
              <Button
                onClick={this.handleToggle('expanded')}
                style={{width: 200}}>
                Hide options
              </Button>
            </ButtonGroup>
            <ButtonGroup>
              <Button
                onClick={this.handleToggleSide('friendly')}
                bsStyle={boolToBsStyle(showSides.friendly)}>
                Friendly
              </Button>
              <Button
                onClick={this.handleToggleSide('abyssal')}
                bsStyle={boolToBsStyle(showSides.abyssal)}>
                Abyssal
              </Button>
            </ButtonGroup>
            <ButtonGroup>
              <Button
                onClick={this.handleToggle('groupShipTypes')}
                bsStyle={boolToBsStyle(groupShipTypes)}>
                Group by Ship Types
              </Button>
            </ButtonGroup>
            <ButtonGroup>
              <Button
                onClick={this.handleToggle('groupRemodels')}
                bsStyle={boolToBsStyle(groupRemodels)}>
                Group Remodels
              </Button>
            </ButtonGroup>
          </ButtonToolbar>
        </Panel>
        <div style={{display: 'flex'}}>
          <div style={{width: '25%'}}>
            <Button
              onClick={this.handleToggle('expanded')}
              style={{
                width: '100%',
                marginLeft: 0,
                marginBottom: 5,
                ...(expanded ? {display: 'none'} : {}),
              }}>
              Show options
            </Button>
            <ShipPicker />
          </div>
          <ShipViewer style={{flex: 1, marginLeft: 8}} />
        </div>
      </div>
    )
  }
}

const ShipsAlbum = connect(
  listOptionsSelector,
  mapDispatchToProps,
)(ShipsAlbumImpl)

export { ShipsAlbum }
