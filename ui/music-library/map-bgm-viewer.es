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
  focusSelector,
} from './selectors'
import { PTyp } from '../../ptyp'
import { Highlightable } from '../common/highlightable'
import { mapDispatchToProps } from '../../store'
import { BgmListItem } from './bgm-list-item'

@connect(
  createStructuredSelector({
    grouppedMapIds: grouppedMapIdsSelector,
    listInfo: focusedListInfoSelector,
    useSiteInfo: mapBgmUseSiteInfoSelector,
    focus: focusSelector,
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
    focus: PTyp.object.isRequired,

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
    const {grouppedMapIds, listInfo, focus} = this.props
    const {__} = window.i18n["poi-plugin-navy-album"]
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
          {
            [
              {
                which: {type: 'all'},
                content: __(`MusicLibraryTab.All`),
              },
              ...grouppedMapIds.map(grp => ({
                which: {type: 'world', worldId: grp.area},
                content: `World #${grp.area}`,
              })),
              {
                which: {type: 'others'},
                content: __(`MusicLibraryTab.Others`),
              },
            ].map(({which, content}) => {
              const key = which.type === 'world' ? which.area : which
              return (
                <Card
                  style={{padding: 0}}
                  key={key}
                  onClick={this.handleChangeFocus(which)}
                >
                  <Highlightable
                    style={{padding: '8px 10px'}}
                    highlight={_.isEqual(which, focus)}
                    content={content}
                  />
                </Card>
              )
            })
          }
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
