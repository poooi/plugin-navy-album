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
import { BgmListItemP2 } from './bgm-list-item-p2'

const getPath = getBgmFilePath('map')

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
    const {__} = window
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
      <BgmListItemP2
        key={key}
        bgmId={bgmId}
        bgmType="battle"
      >
        <div>
          <div>{__('MusicLibraryTab.MapBGM')} #{bgmId}</div>
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
                  _.map(
                    useInfo[curMapId],
                    s => __(`MusicLibraryTab.Situations.${s}`)
                  ),
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
      </BgmListItemP2>
    )
  }

  render() {
    const {grouppedMapIds, listInfo} = this.props
    const {__} = window
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
            {__(`MusicLibraryTab.All`)}
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
            {__(`MusicLibraryTab.Others`)}
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
