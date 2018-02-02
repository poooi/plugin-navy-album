import _ from 'lodash'
import { modifyObject } from 'subtender'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import {
  Panel, Nav, NavItem, Tab,
} from 'react-bootstrap'
import { PTyp } from '../../ptyp'
import { mapDispatchToProps } from '../../store'
import { activeTabSelector } from './selectors'
import { PortBgmViewer } from './port-bgm-viewer'
import { MapBgmViewer } from './map-bgm-viewer'

class MusicLibraryImpl extends PureComponent {
  static propTypes = {
    activeTab: PTyp.string.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  handleSwitchTab = activeTab =>
    this.props.uiModify(
      modifyObject(
        'musicLibrary',
        modifyObject('activeTab', () => activeTab)
      )
    )

  render() {
    const {activeTab} = this.props
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
            style={{
              flex: 1,
              display: 'flex',
              flexDirection: 'column',
              height: '100%',
            }}
            activeKey={activeTab}
            onSelect={this.handleSwitchTab}
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
                <Tab.Content
                  style={{height: '100%'}}
                  animation={false}
                >
                  <Tab.Pane
                    style={{height: '100%'}}
                    eventKey="port"
                  >
                    <PortBgmViewer />
                  </Tab.Pane>
                  <Tab.Pane
                    style={{height: '100%'}}
                    eventKey="map"
                  >
                    <MapBgmViewer />
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
  createStructuredSelector({
    activeTab: activeTabSelector,
  }),
  mapDispatchToProps,
)(MusicLibraryImpl)

export { MusicLibrary }
