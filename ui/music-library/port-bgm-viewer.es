import _ from 'lodash'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import {
  portBgmsSelector,
} from '../../selectors'
import { PTyp } from '../../ptyp'
import { mapDispatchToProps } from '../../store'
import { BgmListItem } from './bgm-list-item'

@connect(
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
)
class PortBgmViewer extends PureComponent {
  static propTypes = {
    portBgmList: PTyp.array.isRequired,
    onPlay: PTyp.func.isRequired,
  }

  render() {
    const {portBgmList, onPlay} = this.props
    return (
      <div
        style={{
          overflowY: 'auto',
          height: 'calc(100% - 40px)',
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
      </div>
    )
  }
}

export { PortBgmViewer }
