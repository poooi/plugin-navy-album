import React, { Component } from 'react'
import { connect } from 'react-redux'
import { Button } from '@blueprintjs/core'
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
    const boolToIntent = v => v ? 'primary' : 'default'
    return (
      <div
        style={{
          height: '100%',
          display: 'flex',
          flexDirection: 'column',
        }}>
        <div
          style={{
            marginBottom: 8,
            ...(expanded ? {} : {display: 'none'}),
          }}>
          <div
            style={{display: 'flex', width: '100%'}}
          >
            <Button
              onClick={this.handleToggle('expanded')}
              style={{width: '25%'}}
              text={__('HideOpts')}
            />
            <div style={{marginLeft: 5}}>
              <Button
                onClick={this.handleToggleSide('friendly')}
                intent={boolToIntent(showSides.friendly)}
                text={__('Friendly')}
              />
              <Button
                onClick={this.handleToggleSide('abyssal')}
                intent={boolToIntent(showSides.abyssal)}
                text={__('Abyssal')}
              />
            </div>
            <Button
              style={{marginLeft: 5}}
              onClick={this.handleToggle('groupShipTypes')}
              intent={boolToIntent(groupShipTypes)}
              text={__('ShipsTab.GroupByShipTypes')}
            />
            <Button
              style={{marginLeft: 5}}
              onClick={this.handleToggle('groupRemodels')}
              intent={boolToIntent(groupRemodels)}
              text={__('ShipsTab.GroupRemodels')}
            />
          </div>
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
              }}
              text={__('ShowOpts')}
            />
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
