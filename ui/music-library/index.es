import { modifyObject } from 'subtender'
import {
  createStructuredSelector,
} from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import styled from 'styled-components'
import { Tab , Tabs } from '@blueprintjs/core'
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
  const audioTags = [...mountPoint.querySelectorAll('.musiclib-root-tabs audio')]
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

/*
  Setting styles on Tabs / Tab does not seem to work
  (they are probably not being passed down at all),
  therefore going with styled approach.
 */
const MusicLibTabs = styled(Tabs)`
  display: flex;
  flex-direction: column;
  height: 100%;

  & .musiclib-port {
    flex: 1;
    overflow-y: auto;
    height: 100%;
  }

  & .musiclib-map {
    flex: 1;
    height: 100%;
  }
`

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
          <ErrorBoundary>
            <MusicLibTabs
              className="musiclib-root-tabs"
              id="poi-plugin-navy-album-music-library-root"
              selectedTabId={activeTab}
              onChange={this.handleSwitchTab}
              animate={false}
            >
              <Tab
                id="port"
                className="musiclib-port musiclib-tab"
                title={__('MusicLibraryTab.PortBGM')}
                panel={
                  <PortBgmViewer
                    onPlay={mkPlayExclusively(mountPoint)}
                  />
                }
              />
              <Tab
                id="map"
                className="musiclib-map musiclib-tab"
                title={__('MusicLibraryTab.MapBGM')}
                panel={
                  <MapBgmViewer
                    onPlay={mkPlayExclusively(mountPoint)}
                  />
                }
              />
            </MusicLibTabs>
          </ErrorBoundary>
        )}
      </WindowEnv.Consumer>
    )
  }
}

export { MusicLibrary }
