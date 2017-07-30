import React, { Component } from 'react'
import {
  Tabs, Tab, Row, Col,
  Nav, NavItem,
} from 'react-bootstrap'
import { connect } from 'react-redux'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import { modifyObject } from 'subtender'

import { uiSelector } from '../selectors'
import { Placeholder } from './placeholer'
import { PTyp } from '../ptyp'
import { mapDispatchToProps } from '../store'

class NavyAlbumImpl extends Component {
  static propTypes = {
    activeTab: PTyp.ActiveTab.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  handleSwitchTab = activeTab =>
    this.props.uiModify(
      modifyObject('activeTab', () => activeTab))

  render() {
    const {activeTab} = this.props
    return (
      <Tab.Container
        id="na-main-tab"
        onSelect={this.handleSwitchTab}
        activeKey={activeTab}>
        <div>
          <div style={{marginBottom: 5}}>
            <Nav
              bsStyle="tabs"
              justified className="main-nav">
              <NavItem eventKey="ships">
                Ships
              </NavItem>
              <NavItem eventKey="equipments">
                Equipments
              </NavItem>
            </Nav>
          </div>
          <div>
            <Tab.Content>
              <Tab.Pane eventKey="ships">
                <Placeholder />
              </Tab.Pane>
              <Tab.Pane eventKey="equipments">
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
  }),
  mapDispatchToProps,
)(NavyAlbumImpl)

export { NavyAlbum }
