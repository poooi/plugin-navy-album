import React, { Component } from 'react'
import { connect } from 'react-redux'
import { Button, ButtonGroup, ButtonToolbar } from 'react-bootstrap'
import {
  modifyObject,
  not,
} from 'subtender'

import { listOptionsSelector } from './selectors'
import { mapDispatchToProps } from '../../store'
import { PTyp } from '../../ptyp'

import { ShipPicker } from './ship-picker'
import { ShipViewer } from './ship-viewer'

@connect(
  listOptionsSelector,
  mapDispatchToProps,
)
class ShipsAlbum extends Component {
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
    const {__} = window.i18n["poi-plugin-navy-album"]
    const boolToBsStyle = v => v ? 'primary' : 'default'
    return (
      <div style={{height: '100%', display: 'flex', flexDirection: 'column'}}>
        <div
          style={{
            marginBottom: 8,
            ...(expanded ? {} : {display: 'none'}),
          }}>
          <ButtonToolbar>
            <ButtonGroup>
              <Button
                onClick={this.handleToggle('expanded')}
                style={{width: 200}}>
                {__('HideOpts')}
              </Button>
            </ButtonGroup>
            <ButtonGroup>
              <Button
                onClick={this.handleToggleSide('friendly')}
                bsStyle={boolToBsStyle(showSides.friendly)}>
                {__('Friendly')}
              </Button>
              <Button
                onClick={this.handleToggleSide('abyssal')}
                bsStyle={boolToBsStyle(showSides.abyssal)}>
                {__('Abyssal')}
              </Button>
            </ButtonGroup>
            <ButtonGroup>
              <Button
                onClick={this.handleToggle('groupShipTypes')}
                bsStyle={boolToBsStyle(groupShipTypes)}>
                {__('ShipsTab.GroupByShipTypes')}
              </Button>
            </ButtonGroup>
            <ButtonGroup>
              <Button
                onClick={this.handleToggle('groupRemodels')}
                bsStyle={boolToBsStyle(groupRemodels)}>
                {__('ShipsTab.GroupRemodels')}
              </Button>
            </ButtonGroup>
          </ButtonToolbar>
        </div>
        <div
          style={{
            display: 'flex',
            flex: 1,
            height: 0,
          }}
        >
          <div style={{
            width: '25%',
            minWidth: '14em', maxWidth: '21em',
            display: 'flex',
            flexDirection: 'column',
          }}>
            <Button
              onClick={this.handleToggle('expanded')}
              style={{
                width: '100%',
                marginLeft: 0,
                marginBottom: 5,
                ...(expanded ? {display: 'none'} : {}),
              }}>
              {__('ShowOpts')}
            </Button>
            <ShipPicker />
          </div>
          <ShipViewer style={{
            minWidth: 500,
            flex: 1, marginLeft: 8, marginBottom: 8,
          }} />
        </div>
      </div>
    )
  }
}

export { ShipsAlbum }
