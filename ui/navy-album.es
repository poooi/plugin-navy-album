import React, { Component } from 'react'
import {
  Tab, Nav, NavItem,
} from 'react-bootstrap'
import { connect } from 'react-redux'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import { modifyObject } from 'subtender'

import { uiSelector, themeSelector } from '../selectors'
import { PTyp } from '../ptyp'
import { mapDispatchToProps } from '../store'
import { ShipsAlbum } from './ships-album'
import { EquipmentsAlbum } from './equipments-album'
import { MusicLibrary } from './music-library'
import { GameUpdateViewer } from './game-update-viewer'

class NavyAlbumImpl extends Component {
  static propTypes = {
    activeTab: PTyp.ActiveTab.isRequired,
    theme: PTyp.string.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  handleSwitchTab = activeTab =>
    this.props.uiModify(
      modifyObject('activeTab', () => activeTab)
    )

  render() {
    const {activeTab, theme} = this.props
    const {__} = window.i18n["poi-plugin-navy-album"]
    return (
      <Tab.Container
        id="na-main-tab"
        className={`theme-${theme}`}
        onSelect={this.handleSwitchTab}
        style={{
          height: '100vh',
          display: 'flex',
          flexDirection: 'column',
        }}
        activeKey={activeTab}>
        <div>
          <div style={{
            marginBottom: 8,
          }}>
            <Nav
              style={{
                // left & right margin = 1%
                width: '98vw',
              }}
              bsStyle="tabs"
              className="main-nav">
              <NavItem eventKey="ships">
                {__('Ships')}
              </NavItem>
              <NavItem eventKey="equipments">
                {__('Equipments')}
              </NavItem>
              <NavItem eventKey="music">
                {__('Music')}
              </NavItem>
              <NavItem eventKey="game-update">
                {__('GameUpdate')}
              </NavItem>
            </Nav>
          </div>
          <div style={{flex: 1, height: 0}}>
            <Tab.Content
              animation={false}
              style={{height: '100%'}}
            >
              <Tab.Pane eventKey="ships" style={{height: '100%'}}>
                <ShipsAlbum />
              </Tab.Pane>
              <Tab.Pane eventKey="equipments" style={{height: '100%'}}>
                <EquipmentsAlbum />
              </Tab.Pane>
              <Tab.Pane eventKey="music" style={{height: '100%'}}>
                <MusicLibrary />
              </Tab.Pane>
              <Tab.Pane eventKey="game-update" style={{height: '100%'}}>
                <GameUpdateViewer />
              </Tab.Pane>
            </Tab.Content>
          </div>
        </div>
      </Tab.Container>
    )
  }
}

const NavyAlbum = connect(
  createStructuredSelector({
    activeTab: createSelector(
      uiSelector,
      ui => ui.activeTab),
    theme: themeSelector,
  }),
  mapDispatchToProps,
)(NavyAlbumImpl)

export { NavyAlbum }
