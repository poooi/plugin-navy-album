import React, { Component } from 'react'
import { connect } from 'react-redux'
import styled from 'styled-components'
import { Card, Text, Classes } from '@blueprintjs/core'
import { mergeMapStateToProps, modifyObject } from 'subtender'
import { SlotitemIcon } from 'views/components/etc/icon'

import {
  equipsRawSelectorForView,
  searchTextSelector,
  mstIdSelector,
} from './selectors'

import { PTyp } from '../../ptyp'
import { mapDispatchToProps } from '../../store'
import { SearchBar } from '../search-bar'

const EqpIcon = styled(SlotitemIcon)`
  &.svg {
    height: 2em;
  }

  &.png {
    height: 2em;
    margin-right: -0.2em;
  }
`

const highlightProps = {
  className: `${Classes.CALLOUT} ${Classes.INTENT_PRIMARY}`,
}

@connect(
  mergeMapStateToProps(
    equipsRawSelectorForView,
    state => ({
      searchText: searchTextSelector(state),
      viewingMstId: mstIdSelector(state),
    })),
  mapDispatchToProps,
)
class EquipPicker extends Component {
  static propTypes = {
    wrappedEquipsRaw: PTyp.array.isRequired,
    searchText: PTyp.string.isRequired,
    viewingMstId: PTyp.number.isRequired,
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
    const {wrappedEquipsRaw,searchText,viewingMstId} = this.props
    return (
      <div
        className="equip-picker"
        style={{
          margin: 10,
          display: 'flex',
          flexDirection: 'column',
          height: 0,
          flex: 1,
        }}>
        <SearchBar
          style={{marginBottom: 8}}
          value={searchText}
          changeValue={this.handleChangeSearchText}
        />
        <div style={{
          flex: 1,
          overflowY: 'auto',
          height: '100%',
        }}>
          {
            wrappedEquipsRaw.map(wrapped => {
              let key
              let content
              let onClick = null
              let shouldHighlight = false
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
                    <EqpIcon slotitemId={icon} />
                    <span style={{marginLeft: '.2em'}}>
                      {name} ({mstId})
                    </span>
                  </div>
                )
                if (mstId === viewingMstId) {
                  shouldHighlight = true
                }
                onClick = this.handleSelectMstId(mstId)
              }
              return (
                <Card
                  key={key}
                  style={{padding: 0}}
                  onClick={onClick}
                  className="equip-picker-item">
                  <Text
                    style={{
                      paddingTop: '.4em',
                      paddingBottom: '.4em',
                      paddingLeft: '.4em',
                    }}
                    {...(shouldHighlight ? highlightProps : {})}
                  >
                    {content}
                  </Text>
                </Card>
              )
            })
          }
        </div>
      </div>
    )
  }
}

export { EquipPicker }
