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
import { Placeholder } from './placeholer'
import { PTyp } from '../ptyp'
import { mapDispatchToProps } from '../store'
import { ShipsAlbum } from './ships-album'
import { observeAll } from '../observers'

class NavyAlbumImpl extends Component {
  static propTypes = {
    activeTab: PTyp.ActiveTab.isRequired,
    theme: PTyp.string.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  constructor(props) {
    super(props)
    this.unsubscribe = observeAll()
  }

  componentWillUnmount() {
    if (typeof this.unsubscribe !== 'function') {
      console.warn(`unsubscribe is not a function`)
    } else {
      this.unsubscribe()
      this.unsubscribe = null
    }
  }

  handleSwitchTab = activeTab =>
    this.props.uiModify(
      modifyObject('activeTab', () => activeTab))

  render() {
    const {activeTab, theme} = this.props
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
                Ships
              </NavItem>
              <NavItem eventKey="equipments">
                Equipments
              </NavItem>
            </Nav>
          </div>
          <div style={{flex: 1}}>
            <Tab.Content style={{height: '100%'}}>
              <Tab.Pane eventKey="ships" style={{height: '100%'}}>
                <ShipsAlbum />
              </Tab.Pane>
              <Tab.Pane eventKey="equipments" style={{height: '100%'}}>
                <Placeholder />
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
