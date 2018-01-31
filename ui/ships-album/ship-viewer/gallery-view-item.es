import React, { PureComponent } from 'react'
import {
  ListGroupItem, Button,
} from 'react-bootstrap'
import FontAwesome from 'react-fontawesome'

import { PTyp } from '../../../ptyp'

const downloadUrl =
  window.remote.getCurrentWebContents().downloadURL

class GalleryViewItem extends PureComponent {
  static propTypes = {
    src: PTyp.string.isRequired,
    mstId: PTyp.number.isRequired,
    chId: PTyp.number.isRequired,
    lastFetch: PTyp.number.isRequired,
  }

  render() {
    const {
      src, mstId, chId,
      lastFetch,
    } = this.props
    return (
      <ListGroupItem
        style={{
          textAlign: 'center',
        }}>
        <img
          style={{maxWidth: '100%', height: 'auto'}}
          src={`${src}#${lastFetch}`}
          alt={`ship=${mstId}, chId=${chId}`}
        />
        {
          src && (
            <div style={{display: 'flex', flexDirection: 'row-reverse'}}>
              <Button
                onClick={() => downloadUrl(src)}
                bsSize="small">
                <FontAwesome name="download" />
              </Button>
            </div>
          )
        }
      </ListGroupItem>
    )
  }
}

export { GalleryViewItem }
