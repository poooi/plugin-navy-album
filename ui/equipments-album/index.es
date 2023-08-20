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
import { EquipPicker } from './equip-picker'
import { EquipViewer } from './equip-viewer'

@connect(
  listOptionsSelector,
  mapDispatchToProps,
)
class EquipmentsAlbum extends Component {
  static propTypes = {
    expanded: PTyp.bool.isRequired,
    showSides: PTyp.object.isRequired,
    groupEquipTypes: PTyp.bool.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  modifyListOptions = modifier =>
    this.props.uiModify(
      modifyObject(
        'equipmentsAlbum',
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
      groupEquipTypes,
    } = this.props

    const {__} = window.i18n["poi-plugin-navy-album"]

    const boolToIntent = v => v ? 'primary' : 'default'
    return (
      <div
        style={{
          height: '100%',
          display: 'flex',
          flexDirection: 'column',
        }}
      >
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
              style={{width: '30%'}}
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
              onClick={this.handleToggle('groupEquipTypes')}
              intent={boolToIntent(groupEquipTypes)}
              text={__('EquipmentsTab.GroupByTypes')}
              style={{marginLeft: 5}}
            />
          </div>
        </div>
        <div style={{display: 'flex', flex: 1, height: 0}}>
          <div style={{
            width: '30%',
            minWidth: '14em', maxWidth: '25em',
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
            <EquipPicker />
          </div>
          <EquipViewer style={{
            minWidth: 400,
            flex: 1, margin: '10px 20px',
          }} />
        </div>
      </div>
    )
  }
}

export { EquipmentsAlbum }
