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
import { PortBgmViewer } from './port-bgm-viewer'
import { getBgmFilePath } from '../../swf-cache'

const getPath = getBgmFilePath('port')

class MusicLibraryImpl extends PureComponent {
  static propTypes = {
  }

  render() {
    return (
      <PortBgmViewer />
    )
  }
}

const MusicLibrary = connect(
)(MusicLibraryImpl)

export { MusicLibrary }
