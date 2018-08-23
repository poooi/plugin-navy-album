import _ from 'lodash'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { ListGroup } from 'react-bootstrap'
import {
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
    portBgmCache: PTyp.object.isRequired,
    requestBgm: PTyp.func.isRequired,
    isFetching: PTyp.func.isRequired,
  }

  handleRequestBgm = id => forced =>
    this.props.requestBgm('port', id, forced)

  render() {
    const {portBgmList, portBgmCache, isFetching} = this.props
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
            return (
              <BgmListItem
                key={id}
                bgmId={id}
                bgmType="port"
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
    portBgmCache: createSelector(
      swfCacheSelector,
      sc => sc.portBgm
    ),
    isFetching: createSelector(
      swfCacheSelector,
      swfCache => bgmId => {
        const {fetchLocks} = swfCache
        return fetchLocks.some(urlPath => {
          const reResult = /^\/kcs\/resources\/bgm_p\/(\d+).*\.swf$/.exec(urlPath)
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
)(PortBgmViewerImpl)

export { PortBgmViewer }
