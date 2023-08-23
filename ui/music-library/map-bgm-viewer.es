import _ from 'lodash'
import { createStructuredSelector } from 'reselect'
import React, { PureComponent } from 'react'
import { modifyObject } from 'subtender'
import { mapIdToStr } from 'subtender/kc'
import { Card } from '@blueprintjs/core'
import { connect } from 'react-redux'

import {
  grouppedMapIdsSelector,
  mapBgmUseSiteInfoSelector,
} from '../../selectors'
import {
  focusedListInfoSelector,
} from './selectors'
import { PTyp } from '../../ptyp'
import { mapDispatchToProps } from '../../store'
import { BgmListItem } from './bgm-list-item'

/*
  TODO: highlight selected item.
 */

@connect(
  createStructuredSelector({
    grouppedMapIds: grouppedMapIdsSelector,
    listInfo: focusedListInfoSelector,
    useSiteInfo: mapBgmUseSiteInfoSelector,
  }),
  mapDispatchToProps,
)
class MapBgmViewer extends PureComponent {
  static propTypes = {
    onPlay: PTyp.func.isRequired,

    // connected
    grouppedMapIds: PTyp.array.isRequired,
    listInfo: PTyp.object.isRequired,
    useSiteInfo: PTyp.object.isRequired,

    uiModify: PTyp.func.isRequired,
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

  mkBgmListItem = (bgmId, key, mapId = null) => {
    const {__} = window.i18n["poi-plugin-navy-album"]
    const {useSiteInfo, onPlay} = this.props
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
        bgmId={bgmId}
        onPlay={onPlay}
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
      </BgmListItem>
    )
  }

  render() {
    const {grouppedMapIds, listInfo} = this.props
    const {__} = window.i18n["poi-plugin-navy-album"]
    const itemStyle = {padding: '8px 10px'}
    return (
      <div
        style={{
          height: 'calc(100% - 80px)',
          display: 'flex',
        }}
      >
        <div
          style={{
            width: '20%',
            maxWidth: '10em',
            marginBottom: 0,
            height: '100%',
            overflowY: 'auto',
          }}
        >
          <Card
            style={itemStyle}
            onClick={this.handleChangeFocus({type: 'all'})}
          >
            {__(`MusicLibraryTab.All`)}
          </Card>
          {
            grouppedMapIds.map(grp => (
              <Card
                onClick={this.handleChangeFocus({type: 'world', worldId: grp.area})}
                style={itemStyle}
                key={grp.area}
              >
                World #{grp.area}
              </Card>
            ))
          }
          <Card
            onClick={this.handleChangeFocus({type: 'others'})}
            style={itemStyle}
          >
            {__(`MusicLibraryTab.Others`)}
          </Card>
        </div>
        <div
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
        </div>
      </div>
    )
  }
}

export {
  MapBgmViewer,
}
