import { remote } from 'electron'
import React, { PureComponent } from 'react'
import {
  ListGroupItem,
} from 'react-bootstrap'
import { connect } from 'react-redux'
import { PTyp } from '../../ptyp'
import { getBgm } from '../../game-misc'

import {
  poiVolumeSelector,
  serverIpSelector,
} from '../../selectors'

const downloadUrl =
  remote.getCurrentWebContents().downloadURL

class BgmListItemImpl extends PureComponent {
  static propTypes = {
    children: PTyp.node.isRequired,
    bgmId: PTyp.number.isRequired,
    bgmType: PTyp.string.isRequired,

    // connected:
    volume: PTyp.number.isRequired,
    serverIp: PTyp.string.isRequired,
  }

  handleCanPlay = e => {
    e.target.volume = this.props.volume
  }

  /*
     pause all audio tags except one that we just started playing,
     this prevents more than one music to be played at the same time.
   */
  handlePlaying = e => {
    const currentAudio = e.target
    const {$$} = window
    // TODO: no longer working after migrating to embedded plugin.
    const audioTags = [...$$('#poi-plugin-navy-album audio')]
    audioTags.map(aud => {
      if (aud === currentAudio)
        return

      // https://stackoverflow.com/a/6877530
      // https://stackoverflow.com/a/31133401
      if (
        aud.currentTime > 0 &&
        !aud.paused &&
        !aud.ended &&
        aud.readyState > 2
      ) {
        aud.pause()
      }
    })
  }

  handleDownload = path => () => downloadUrl(`file://${path}`)

  render() {
    const {children, serverIp, bgmId, bgmType} = this.props
    const path = getBgm(bgmId, bgmType)
    const url = `http://${serverIp}${path}`
    return (
      <ListGroupItem
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
            onPlaying={this.handlePlaying}
            controls="controls">
            <source src={url} type="audio/mp3" />
          </audio>
        </div>
      </ListGroupItem>
    )
  }
}

const BgmListItem = connect(
  state => ({
    volume: poiVolumeSelector(state),
    serverIp: serverIpSelector(state),
  }),
)(BgmListItemImpl)

export { BgmListItem }
