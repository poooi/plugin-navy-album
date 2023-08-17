import { remote } from 'electron'
import React, { PureComponent } from 'react'
import { Card } from '@blueprintjs/core'
import { connect } from 'react-redux'
import { PTyp } from '../../ptyp'
import { getBgm } from '../../game-misc'

import {
  poiVolumeSelector,
  serverIpSelector,
} from '../../selectors'

const downloadUrl =
  remote.getCurrentWebContents().downloadURL

@connect(
  state => ({
    volume: poiVolumeSelector(state),
    serverIp: serverIpSelector(state),
  }),
)
class BgmListItem extends PureComponent {
  static propTypes = {
    children: PTyp.node.isRequired,
    bgmId: PTyp.number.isRequired,
    bgmType: PTyp.string.isRequired,
    onPlay: PTyp.func.isRequired,

    // connected:
    volume: PTyp.number.isRequired,
    serverIp: PTyp.string.isRequired,
  }

  handleCanPlay = e => {
    e.target.volume = this.props.volume
  }

  handleDownload = path => () => downloadUrl(`file://${path}`)

  render() {
    const {children, serverIp, bgmId, bgmType, onPlay} = this.props
    const path = getBgm(bgmId, bgmType)
    const url = `http://${serverIp}${path}`
    return (
      <Card
        style={{
          display: 'flex', flexDirection: 'column',
          padding: '5px 10px',
        }}
      >
        <div
          style={{display: 'flex', alignItems: 'center'}}
        >
          <div style={{flex: 1}}>{children}</div>
        </div>
        <div
          style={{
            display: 'flex', alignItems: 'center',
            marginTop: '.5em',
          }}
        >
          <audio
            loop
            className="play-control"
            style={{
              flex: 1,
            }}
            preload="none"
            onCanPlay={this.handleCanPlay}
            onPlaying={onPlay}
            controls="controls">
            <source src={url} type="audio/mp3" />
          </audio>
        </div>
      </Card>
    )
  }
}

export { BgmListItem }
