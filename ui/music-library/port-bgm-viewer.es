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
import { BgmListItem } from './bgm-list-item'

const getPath = getBgmFilePath('port')

class PortBgmViewerImpl extends PureComponent {
  static propTypes = {
    portBgmList: PTyp.array.isRequired,
    volume: PTyp.number.isRequired,
    portBgmCache: PTyp.object.isRequired,
    requestBgm: PTyp.func.isRequired,
  }

  handleCanPlay = e => {
    e.target.volume = this.props.volume
  }

  handleRequestBgm = id => forced =>
    this.props.requestBgm('port', id, forced)

  render() {
    const {portBgmList, portBgmCache, volume} = this.props
    return (
      <ListGroup
        style={{
          overflowY: 'auto',
          height: '100%',
          marginBottom: 0,
        }}
      >
        {
          portBgmList.map(({id, name}) => {
            const cacheHit = !_.isEmpty(portBgmCache[id])
            const maybePath = cacheHit ? getPath(id) : null
            return (
              <BgmListItem
                key={id}
                volume={volume}
                maybePath={maybePath}
                onRequestBgm={this.handleRequestBgm(id)}
              >
                {`${name} (${id})`}
              </BgmListItem>
            )
          })
        }
      </ListGroup>
    )
  }
}

const PortBgmViewer = connect(
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
)(PortBgmViewerImpl)

export { PortBgmViewer }
