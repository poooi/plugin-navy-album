import React, { PureComponent } from 'react'
import {
  ListGroupItem, Button,
} from 'react-bootstrap'
import FontAwesome from 'react-fontawesome'
import { remote } from 'electron'
import { extname } from 'path-extra'
import { copySync } from 'fs-extra'

import { PTyp } from '../../../ptyp'

const {dialog} = remote.require('electron')

const handleSaveImage = (src, mstId, chId, lastFetch) => () => {
  const ext = extname(src)
  const defFileName = `${mstId}-${chId}-${lastFetch}${ext}`
  dialog.showSaveDialog(
    {defaultPath: defFileName},
    fileName => {
      if (!fileName)
        return
      try {
        copySync(src, fileName)
      } catch (e) {
        console.error(`error while saving file: `, e)
      }
    }
  )
}

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
                onClick={handleSaveImage(src,mstId,chId, lastFetch)}
                bsSize="small">
                <FontAwesome name="save" />
              </Button>
            </div>
          )
        }
      </ListGroupItem>
    )
  }
}

export { GalleryViewItem }
