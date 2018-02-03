import React, { PureComponent } from 'react'
import {
  ListGroupItem,
  Button,
} from 'react-bootstrap'
import FontAwesome from 'react-fontawesome'
import { PTyp } from '../../ptyp'

class BgmListItem extends PureComponent {
  static propTypes = {
    volume: PTyp.number.isRequired,
    children: PTyp.node.isRequired,
    maybePath: PTyp.string,
    // onRequestBgm(pathAvailable): callback for requesting bgm
    onRequestBgm: PTyp.func.isRequired,
  }

  static defaultProps = {
    maybePath: null,
  }

  handleRequestBgm = pathAvailable => () =>
    this.props.onRequestBgm(pathAvailable)

  handleCanPlay = e => {
    e.target.volume = this.props.volume
  }

  render() {
    const {
      maybePath, children,
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
            onClick={this.handleRequestBgm(pathAvailable)}
            style={{width: '4em', marginTop: 0, alignSelf: 'flex-start'}}
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
              controls="controls">
              <source src={maybePath} type="audio/mp3" />
            </audio>
          )
        }
      </ListGroupItem>
    )
  }
}

export { BgmListItem }
