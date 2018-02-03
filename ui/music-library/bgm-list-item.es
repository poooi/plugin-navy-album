import React, { PureComponent } from 'react'
import {
  ListGroupItem,
  Button,
} from 'react-bootstrap'
import { connect } from 'react-redux'
import FontAwesome from 'react-fontawesome'
import { PTyp } from '../../ptyp'

import {
  poiVolumeSelector,
} from '../../selectors'

class BgmListItemImpl extends PureComponent {
  static propTypes = {
    children: PTyp.node.isRequired,
    maybePath: PTyp.string,
    // onRequestBgm(pathAvailable): callback for requesting bgm
    onRequestBgm: PTyp.func.isRequired,
    isFetching: PTyp.bool,

    // connected:
    volume: PTyp.number.isRequired,
  }

  static defaultProps = {
    maybePath: null,
    isFetching: false,
  }

  handleRequestBgm = pathAvailable => () =>
    this.props.onRequestBgm(pathAvailable)

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

    const audioTags = [...$$('#content-root audio')]
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

  render() {
    const {
      maybePath, children, isFetching,
    } = this.props
    const pathAvailable = Boolean(maybePath)
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
          <Button
            bsSize="small"
            onClick={this.handleRequestBgm(pathAvailable)}
            style={{marginTop: 0, alignSelf: 'flex-start'}}
            disabled={isFetching}
          >
            <FontAwesome
              name={pathAvailable ? 'refresh' : 'download'}
            />
          </Button>
        </div>
        {
          pathAvailable && (
            <audio
              className="play-control"
              style={{width: '100%', marginTop: '.5em'}}
              preload="none"
              onCanPlay={this.handleCanPlay}
              onPlaying={this.handlePlaying}
              controls="controls">
              <source src={maybePath} type="audio/mp3" />
            </audio>
          )
        }
      </ListGroupItem>
    )
  }
}

const BgmListItem = connect(
  state => ({volume: poiVolumeSelector(state)}),
)(BgmListItemImpl)

export { BgmListItem }
