import _ from 'lodash'
import { createSelector, createStructuredSelector } from 'reselect'
import React, { PureComponent } from 'react'
import { modifyObject } from 'subtender'
import { mapIdToStr } from 'subtender/kc'
import {
  ListGroup,
  ListGroupItem,
} from 'react-bootstrap'
import { connect } from 'react-redux'

import {
  grouppedMapIdsSelector,
  swfCacheSelector,
} from '../../selectors'
import {
  focusedListInfoSelector,
} from './selectors'
import { PTyp } from '../../ptyp'
import { mapDispatchToProps } from '../../store'
import { getBgmFilePath } from '../../swf-cache'
import { BgmListItem } from './bgm-list-item'

const getPath = getBgmFilePath('map')

class MapBgmViewerImpl extends PureComponent {
  static propTypes = {
    grouppedMapIds: PTyp.array.isRequired,
    listInfo: PTyp.object.isRequired,
    mapBgmCache: PTyp.object.isRequired,
    uiModify: PTyp.func.isRequired,

    requestBgm: PTyp.func.isRequired,
  }

  handleChangeFocus = focus => () =>
    this.props.uiModify(
      modifyObject(
        'musicLibrary',
        modifyObject(
          'mapBgmViewer',
          modifyObject('focus', () => focus)
        )
      )
    )

  handleRequestBgm = id => forced =>
    this.props.requestBgm('map', id, forced)

  mkBgmListItem = (id, key) => {
    const {mapBgmCache} = this.props
    const cacheHit = !_.isEmpty(mapBgmCache[id])
    const maybePath = cacheHit ? getPath(id) : null
    return (
      <BgmListItem
        key={key}
        maybePath={maybePath}
        onRequestBgm={this.handleRequestBgm(id)}
      >
        {id}
      </BgmListItem>
    )
  }

  render() {
    const {grouppedMapIds, listInfo} = this.props
    const itemStyle = {padding: '8px 10px'}
    return (
      <div
        style={{
          height: '100%',
          display: 'flex',
        }}
      >
        <ListGroup
          style={{
            width: '20%',
            maxWidth: '10em',
            marginBottom: 0,
            height: '100%',
            overflowY: 'auto',
          }}
        >
          <ListGroupItem
            style={itemStyle}
            onClick={this.handleChangeFocus({type: 'all'})}
          >
            All
          </ListGroupItem>
          {
            grouppedMapIds.map(grp => (
              <ListGroupItem
                onClick={this.handleChangeFocus({type: 'world', worldId: grp.area})}
                style={itemStyle}
                key={grp.area}
              >
                World #{grp.area}
              </ListGroupItem>
            ))
          }
          <ListGroupItem
            onClick={this.handleChangeFocus({type: 'others'})}
            style={itemStyle}
          >
            Others
          </ListGroupItem>
        </ListGroup>
        <ListGroup
          style={{
            flex: 1,
            height: '100%',
            marginBottom: 0,
            marginLeft: 5,
            overflowY: 'auto',
          }}
        >
          {
            listInfo.type === 'simple' ? (
              listInfo.list.map(bgmId => this.mkBgmListItem(bgmId, `simple-${bgmId}`))
            ) : (
              <div>
                {
                  listInfo.list.map(({mapId, bgmIds}) => (
                    <div
                      key={mapId}
                    >
                      <h4>{mapIdToStr(mapId)}</h4>
                      <div style={{marginLeft: 5}}>
                        {
                          bgmIds.map(bgmId =>
                            this.mkBgmListItem(bgmId, `groupped-${bgmId}`)
                          )
                        }
                      </div>
                    </div>
                  ))
                }
              </div>
            )
          }
        </ListGroup>
      </div>
    )
  }
}

const MapBgmViewer = connect(
  createStructuredSelector({
    grouppedMapIds: grouppedMapIdsSelector,
    listInfo: focusedListInfoSelector,
    mapBgmCache: createSelector(
      swfCacheSelector,
      sc => sc.mapBgm
    ),
  }),
  mapDispatchToProps,
)(MapBgmViewerImpl)

export {
  MapBgmViewer,
}
