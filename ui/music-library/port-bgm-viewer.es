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
} from '../../selectors'
import { PTyp } from '../../ptyp'
import { mapDispatchToProps } from '../../store'
import { BgmListItem } from './bgm-list-item'

class PortBgmViewerImpl extends PureComponent {
  static propTypes = {
    portBgmList: PTyp.array.isRequired,
    onPlay: PTyp.func.isRequired,
  }

  render() {
    const {portBgmList, onPlay} = this.props
    return (
      <ListGroup
        style={{
          overflowY: 'auto',
          height: '100%',
          marginBottom: 0,
        }}
      >
        {
          portBgmList.map(({id, name}) =>
            (
              <BgmListItem
                key={id}
                bgmId={id}
                bgmType="port"
                onPlay={onPlay}
              >
                {`${name} (${id})`}
              </BgmListItem>
            )
          )
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
  }),
  mapDispatchToProps,
)(PortBgmViewerImpl)

export { PortBgmViewer }
