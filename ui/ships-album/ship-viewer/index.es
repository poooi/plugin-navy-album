import React, { Component } from 'react'
import { createStructuredSelector } from 'reselect'
import { connect } from 'react-redux'
import {
  Panel, Tab, Nav, NavItem,
} from 'react-bootstrap'
import { modifyObject } from 'subtender'

import { PTyp } from '../../../ptyp'
import { activeTabSelector } from './selectors'
import { Header } from './header'
import { mapDispatchToProps } from '../../../store'

class ShipViewerImpl extends Component {
  static propTypes = {
    style: PTyp.object.isRequired,
    activeTab: PTyp.string.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  handleSwitchTab = activeTab =>
    this.props.uiModify(
      modifyObject(
        'shipsAlbum',
        modifyObject(
          'shipViewer',
          modifyObject(
            'activeTab', () => activeTab
          )
        )
      )
    )

  render() {
    const {style, activeTab} = this.props
    return (
      <Panel
        style={style}>
        <Header />
        <Tab.Container
          id="na-main-tab"
          onSelect={this.handleSwitchTab}
          activeKey={activeTab}>
          <div>
            <div style={{marginBottom: 8}}>
              <Nav
                bsStyle="tabs"
                justified className="main-nav">
                <NavItem eventKey="info">
                  Info
                </NavItem>
                <NavItem eventKey="image">
                  Image
                </NavItem>
                <NavItem eventKey="voice">
                  Voice
                </NavItem>
              </Nav>
            </div>
            <div>
              <Tab.Content>
                <Tab.Pane eventKey="info">
                  TODO: general info
                </Tab.Pane>
                <Tab.Pane eventKey="image">
                  TODO: image viewer
                </Tab.Pane>
                <Tab.Pane eventKey="voice">
                  TODO: voice player & subtitles
                </Tab.Pane>
              </Tab.Content>
            </div>
          </div>
        </Tab.Container>
      </Panel>
    )
  }
}

const ShipViewer = connect(
  createStructuredSelector({
    activeTab: activeTabSelector,
  }),
  mapDispatchToProps,
)(ShipViewerImpl)

export { ShipViewer }
