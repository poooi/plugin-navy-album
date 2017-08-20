import React, { Component } from 'react'
import { connect } from 'react-redux'
import {
  ListGroup, ListGroupItem,
  Panel,
} from 'react-bootstrap'
import { mergeMapStateToProps, modifyObject } from 'subtender'
import { SlotitemIcon } from 'views/components/etc/icon'

import {
  equipsRawSelectorForView,
  searchTextSelector,
} from './selectors'

import { PTyp } from '../../ptyp'
import { mapDispatchToProps } from '../../store'
import { SearchBar } from '../search-bar'

class EquipPickerImpl extends Component {
  static propTypes = {
    wrappedEquipsRaw: PTyp.array.isRequired,
    searchText: PTyp.string.isRequired,
    uiSwitchEquip: PTyp.func.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  handleSelectMstId = mstId => () =>
    this.props.uiSwitchEquip(mstId)

  handleChangeSearchText = searchText =>
    this.props.uiModify(
      modifyObject(
        'equipmentsAlbum',
        modifyObject(
          'searchText', () => searchText
        )
      )
    )

  render() {
    const {wrappedEquipsRaw,searchText} = this.props
    return (
      <Panel
        className="equip-picker"
        style={{
          height: 0,
          flex: 1,
          marginBottom: 8,
        }}>
        <SearchBar
          style={{marginBottom: 8}}
          value={searchText}
          changeValue={this.handleChangeSearchText}
        />
        <ListGroup style={{
          flex: 1,
          overflowY: 'auto',
        }}>
          {
            wrappedEquipsRaw.map(wrapped => {
              let key
              let content
              let onClick = null
              if (wrapped.type === 'etype') {
                const {typeName, etype} = wrapped
                key = `etype-${etype}`
                content = `${typeName} (${etype})`
              } else {
                const {mstId, name, icon} = wrapped
                key = `equip-${mstId}`
                content = (
                  <div
                    style={{display: 'flex', alignItems: 'center'}}>
                    <SlotitemIcon
                      className="slotitem-img"
                      slotitemId={icon}
                    />
                    <span style={{marginLeft: '.2em'}}>
                      {name} ({mstId})
                    </span>
                  </div>
                )
                onClick = this.handleSelectMstId(mstId)
              }
              return (
                <ListGroupItem
                  key={key}
                  style={{padding: 0}}
                  onClick={onClick}
                  className="equip-picker-item">
                  <div style={{
                    paddingTop: '.4em',
                    paddingBottom: '.4em',
                    paddingLeft: '.4em',
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

const EquipPicker = connect(
  mergeMapStateToProps(
    equipsRawSelectorForView,
    state => ({
      searchText: searchTextSelector(state),
    })),
  mapDispatchToProps,
)(EquipPickerImpl)

export { EquipPicker }
