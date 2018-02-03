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
  mapBgmUseSiteInfoSelector,
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

const describe = {
  moving: 'Moving',
  normalDay: 'Normal Battle (Day)',
  normalNight: 'Normal Battle (Night)',
  bossDay: 'Boss Battle (Day)',
  bossNight: 'Boss Battle (Night)',
}

/*
   TODO:

   - i18n
   - download button
   - button styling & icon

 */

class MapBgmViewerImpl extends PureComponent {
  static propTypes = {
    grouppedMapIds: PTyp.array.isRequired,
    listInfo: PTyp.object.isRequired,
    mapBgmCache: PTyp.object.isRequired,
    uiModify: PTyp.func.isRequired,
    useSiteInfo: PTyp.object.isRequired,

    requestBgm: PTyp.func.isRequired,
    isFetching: PTyp.func.isRequired,
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

  mkBgmListItem = (bgmId, key, mapId = null) => {
    const {mapBgmCache, useSiteInfo, isFetching} = this.props
    const cacheHit = !_.isEmpty(mapBgmCache[bgmId])
    const maybePath = cacheHit ? getPath(bgmId) : null
    const useInfo = useSiteInfo[bgmId]
    const allMapIds = _.sortBy(_.keys(useInfo).map(Number), _.identity)
    // determine order
    let mapIds
    if (mapId > 0) {
      mapIds = [mapId, ...allMapIds.filter(x => x !== mapId)]
    } else {
      mapIds = allMapIds
    }

    return (
      <BgmListItem
        key={key}
        maybePath={maybePath}
        isFetching={isFetching(bgmId)}
        onRequestBgm={this.handleRequestBgm(bgmId)}
      >
        <div>
          <div>Map BGM #{bgmId}</div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              flexWrap: 'wrap',
            }}>
            {
              mapIds.map(curMapId => {
                const mapStr = mapIdToStr(curMapId)
                const description = _.join(
                  _.map(useInfo[curMapId], s => describe[s]),
                  ', '
                )
                return (
                  <span
                    style={{
                      marginRight: '.5em',
                      ...(curMapId === mapId ? {fontWeight: 'bold'} : {}),
                    }}
                    key={curMapId}
                  >
                    {mapStr} {description}
                  </span>
                )
              })
            }
          </div>
        </div>
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
                            this.mkBgmListItem(bgmId, `groupped-${bgmId}`, mapId)
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
    useSiteInfo: mapBgmUseSiteInfoSelector,
    isFetching: createSelector(
      swfCacheSelector,
      swfCache => bgmId => {
        const {fetchLocks} = swfCache
        return fetchLocks.some(urlPath => {
          const reResult = /^\/kcs\/resources\/swf\/sound_b_bgm_(\d+)\.swf$/.exec(urlPath)
          if (reResult) {
            return Number(reResult[1]) === bgmId
          } else {
            return false
          }
        })
      }
    ),
  }),
  mapDispatchToProps,
)(MapBgmViewerImpl)

export {
  MapBgmViewer,
}
