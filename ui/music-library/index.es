import { modifyObject } from 'subtender'
import {
  createStructuredSelector,
} from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { Panel, Nav, NavItem, Tab } from 'react-bootstrap'

import { WindowEnv } from 'views/components/etc/window-env'

import { PTyp } from '../../ptyp'
import { mapDispatchToProps } from '../../store'
import { ErrorBoundary } from '../error-boundary'
import { activeTabSelector } from './selectors'
import { PortBgmViewer } from './port-bgm-viewer'
import { MapBgmViewer } from './map-bgm-viewer'

/*
   pause all audio tags except one that we just started playing,
   this prevents more than one music to be played at the same time.
 */
const mkPlayExclusively = mountPoint => e => {
  const currentAudio = e.target
  const audioTags = [...mountPoint.querySelectorAll('#poi-plugin-navy-album-music-library-root audio')]
  if (audioTags.length === 0) {
    console.warn('Cannot find presence of any audio tag.')
  }
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

@connect(
  createStructuredSelector({
    activeTab: activeTabSelector,
  }),
  mapDispatchToProps,
)
class MusicLibrary extends PureComponent {
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
    const {__} = window.i18n["poi-plugin-navy-album"]
    return (
      <WindowEnv.Consumer>
        {({mountPoint}) => (
          <Panel
            id="poi-plugin-navy-album-music-library-root"
            style={{
              height: 'calc(100% - 10px)',
              marginBottom: 10,
            }}
          >
            <Panel.Body
              style={{
                height: '100%',
                padding: 10,
              }}
            >
              <ErrorBoundary>
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
                          {__('MusicLibraryTab.PortBGM')}
                        </NavItem>
                        <NavItem eventKey="map">
                          {__('MusicLibraryTab.MapBGM')}
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
                          <PortBgmViewer
                            onPlay={mkPlayExclusively(mountPoint)}
                          />
                        </Tab.Pane>
                        <Tab.Pane
                          style={{height: '100%'}}
                          eventKey="map"
                        >
                          <MapBgmViewer
                            onPlay={mkPlayExclusively(mountPoint)}
                          />
                        </Tab.Pane>
                      </Tab.Content>
                    </div>
                  </div>
                </Tab.Container>
              </ErrorBoundary>
            </Panel.Body>
          </Panel>
        )}
      </WindowEnv.Consumer>
    )
  }
}

export { MusicLibrary }
