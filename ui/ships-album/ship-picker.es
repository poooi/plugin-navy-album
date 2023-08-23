import React, { Component } from 'react'
import { mergeMapStateToProps, modifyObject } from 'subtender'
import { connect } from 'react-redux'
import { Card } from '@blueprintjs/core'
import {
  shipsInfoSelectorForView,
  listOptionsSelector,
  searchTextSelector,
  mstIdSelector,
} from './selectors'
import { PTyp } from '../../ptyp'
import { Highlightable } from '../common/highlightable'
import { mapDispatchToProps } from '../../store'
import { SearchBar } from '../search-bar'

@connect(
  mergeMapStateToProps(
    shipsInfoSelectorForView,
    listOptionsSelector,
    state => ({
      mstId: mstIdSelector(state),
      searchText: searchTextSelector(state),
    })
  ),
  mapDispatchToProps,
)
class ShipPicker extends Component {
  static propTypes = {
    groupped: PTyp.bool.isRequired,
    wrappedShipsInfo: PTyp.array.isRequired,
    mstId: PTyp.number.isRequired,
    searchText: PTyp.string.isRequired,
    uiSwitchShip: PTyp.func.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  handleSelectMstId = mstId => () =>
    this.props.uiSwitchShip(mstId)

  handleChangeSearchText = searchText =>
    this.props.uiModify(
      modifyObject(
        'shipsAlbum',
        modifyObject(
          'searchText', () => searchText
        )
      )
    )

  render() {
    const {
      groupped,
      searchText,
      wrappedShipsInfo,
      mstId: viewingMstId,
    } = this.props
    return (
      <div
        className="ship-picker"
        style={{
          margin: 10,
          display: 'flex',
          flexDirection: 'column',
          flex: 1,
          height: 0,
        }}>
        <SearchBar
          style={{marginBottom: 8}}
          value={searchText}
          changeValue={this.handleChangeSearchText}
        />
        <div
          style={{
            flex: 1,
            overflowY: 'auto',
            height: '100%',
          }}
        >
          {
            wrappedShipsInfo.map(wrapped => {
              let key
              let content
              let needPadding
              let onClick = null
              let shouldHighlight = false
              if (wrapped.type === 'stype') {
                const {typeName, stype} = wrapped
                key = `stype-${stype}`
                content = `${typeName} (${stype})`
                needPadding = false
              } else {
                const {mstId, name, sortNo, yomi} = wrapped.info
                key = `mstId-${wrapped.info.mstId}`
                content = `${name} ${sortNo ? '' : yomi} (${mstId})`
                needPadding = groupped
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
                  className="ship-picker-item"
                >
                  <Highlightable
                    style={{
                      paddingTop: '.4em',
                      paddingBottom: '.4em',
                      paddingLeft: '.4em',
                      ...(needPadding ? {paddingLeft: '2em'} : {}),
                    }}
                    highlight={shouldHighlight}
                    content={content}
                  />
                </Card>
              )
            })
          }
        </div>
      </div>
    )
  }
}

export { ShipPicker }
