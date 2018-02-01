import _ from 'lodash'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import FontAwesome from 'react-fontawesome'
import {
  Panel, Nav, NavItem, Tab,
  ListGroup, ListGroupItem, Button,
} from 'react-bootstrap'
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
      <Panel
        style={{
          height: 'calc(100% - 10px)',
          marginBottom: 10,
        }}
      >
        <Panel.Body
          style={{height: '100%'}}
        >
          <Tab.Container
            style={{flex: 1, display: 'flex', flexDirection: 'column', height: '100%'}}
          >
            <div>
              <div style={{marginBottom: 8}}>
                <Nav
                  bsStyle="tabs"
                  justified
                >
                  <NavItem eventKey="port">
                    Port BGM
                  </NavItem>
                  <NavItem eventKey="map">
                    Map BGM
                  </NavItem>
                </Nav>
              </div>
              <div style={{flex: 1, height: 0, overflowY: 'auto'}}>
                <Tab.Content animation={false}>
                  <Tab.Pane eventKey="port">
                    <PortBgmViewer />
                  </Tab.Pane>
                  <Tab.Pane eventKey="map">
                    TODO: MapBgmViewer
                  </Tab.Pane>
                </Tab.Content>
              </div>
            </div>
          </Tab.Container>
        </Panel.Body>
      </Panel>
    )
  }
}

const MusicLibrary = connect(
)(MusicLibraryImpl)

export { MusicLibrary }
