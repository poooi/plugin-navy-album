import React, { Component } from 'react'
import { connect } from 'react-redux'
import {
  Panel,
  ButtonToolbar, Button, ButtonGroup,
} from 'react-bootstrap'

import {
  modifyObject,
  not,
} from 'subtender'

import { listOptionsSelector } from './selectors'
import { mapDispatchToProps } from '../../store'
import { PTyp } from '../../ptyp'

class EquipmentsAlbumImpl extends Component {
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

    const boolToBsStyle = v => v ? 'primary' : 'default'
    return (
      <div style={{height: '100%', display: 'flex', flexDirection: 'column'}}>
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
                onClick={this.handleToggle('groupEquipTypes')}
                bsStyle={boolToBsStyle(groupEquipTypes)}>
                Group by Types
              </Button>
            </ButtonGroup>
          </ButtonToolbar>
        </Panel>
        <div style={{display: 'flex', flex: 1}}>
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
              Show options
            </Button>
            <Panel
              style={{
                height: 0,
                flex: 1,
                marginBottom: 8,
              }}>
              TODO: picker
            </Panel>
          </div>
          <Panel style={{
            minWidth: 500,
            flex: 1, marginLeft: 8, marginBottom: 8,
          }}>
            TODO: viewer
          </Panel>
        </div>
      </div>
    )
  }
}

const EquipmentsAlbum = connect(
  listOptionsSelector,
  mapDispatchToProps,
)(EquipmentsAlbumImpl)

export { EquipmentsAlbum }
