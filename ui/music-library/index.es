import _ from 'lodash'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import FontAwesome from 'react-fontawesome'
import { ListGroup, ListGroupItem, Button } from 'react-bootstrap'
import {
  poiVolumeSelector,
  portBgmsSelector,
  swfCacheSelector,
} from '../../selectors'
import { PTyp } from '../../ptyp'
import { mapDispatchToProps } from '../../store'
import { getBgmFilePath } from '../../swf-cache'

const getPath = getBgmFilePath('port')

class MusicLibraryImpl extends PureComponent {
  static propTypes = {
    portBgmList: PTyp.array.isRequired,
    volume: PTyp.number.isRequired,
    portBgmCache: PTyp.object.isRequired,
    requestBgm: PTyp.func.isRequired,
  }

  handleCanPlay = e => {
    e.target.volume = this.props.volume
  }

  handleRequestBgm = (id, forced) => () =>
    this.props.requestBgm('port', id, forced)

  render() {
    const {portBgmList, portBgmCache} = this.props
    return (
      <ListGroup
        style={{
          overflowY: 'auto',
          height: 'calc(100% - 20px)',
          marginBottom: 20,
        }}
      >
        {
          portBgmList.map(({id, name}) => {
            const cacheHit = !_.isEmpty(portBgmCache[id])
            return (
              <ListGroupItem
                style={{
                  display: 'flex', flexDirection: 'column',
                  padding: '5px 10px',
                }}
                key={id}
              >
                <div
                  style={{display: 'flex', alignItems: 'center'}}
                >
                  <div style={{flex: 1}}>{name}</div>
                  <Button
                    onClick={
                      this.handleRequestBgm(
                        id,
                        /*
                           the request is forced if
                           we already have a cache hit
                         */
                        cacheHit
                      )}
                    style={{width: '4em', marginTop: 0}}
                  >
                    <FontAwesome
                      name={cacheHit ? 'refresh' : 'download'}
                    />
                  </Button>
                </div>
                {
                  cacheHit && (
                    <audio
                      className="play-control"
                      style={{width: '100%', marginTop: '.5em'}}
                      preload="none"
                      onCanPlay={this.handleCanPlay}
                      controls="controls">
                      <source src={getPath(id)} type="audio/mp3" />
                    </audio>
                  )
                }
              </ListGroupItem>
            )
          })
        }
      </ListGroup>
    )
  }
}

const MusicLibrary = connect(
  createStructuredSelector({
    portBgmList: createSelector(
      portBgmsSelector,
      p =>
        _.sortBy(
          _.toPairs(p).map(
            ([bgmIdStr, bgmName]) => ({id: Number(bgmIdStr), name: bgmName})
          ),
          'id'
        )
    ),
    volume: poiVolumeSelector,
    portBgmCache: createSelector(
      swfCacheSelector,
      sc => sc.portBgm
    ),
  }),
  mapDispatchToProps,
)(MusicLibraryImpl)

export { MusicLibrary }
