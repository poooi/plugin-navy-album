import _ from 'lodash'
import React, { Component } from 'react'
import { createStructuredSelector } from 'reselect'
import { connect } from 'react-redux'
import {
  Panel, Tab, Nav, NavItem,
} from 'react-bootstrap'
import { mergeMapStateToProps, modifyObject } from 'subtender'

import { PTyp } from '../../../ptyp'
import {
  shipViewerSelector,
  shipGraphSourcesSelector,
  shipMasterDataSelector,
} from '../selectors'
import { Header } from './header'
import { AltFormSwitcher } from './alt-form-switcher'
import { mapDispatchToProps } from '../../../store'
import { AbyssalInfoView } from './abyssal-info-view'
import { ShipInfoView } from './ship-info-view'
import { GalleryView } from './gallery-view'
import { QuotesView } from './quotes-view'

class ShipViewerImpl extends Component {
  static propTypes = {
    style: PTyp.object.isRequired,
    activeTab: PTyp.string.isRequired,
    mstId: PTyp.number.isRequired,
    shipGraphSources: PTyp.object.isRequired,
    $ship: PTyp.object.isRequired,
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
    const {
      style, activeTab, mstId, $ship,
      shipGraphSources,
    } = this.props
    const {__} = window
    const isAbyssalShip = mstId > 1500
    const isSpecialCG = !isAbyssalShip && (!$ship || !('api_sortno' in $ship))
    const shipGraphSource =
      _.get(shipGraphSources, isAbyssalShip ? 3 : 5, '')
    return (
      <Panel
        className="ship-viewer"
        style={style}
      >
        <Header />
        <AltFormSwitcher />
        <Tab.Container
          style={{flex: 1, display: 'flex', flexDirection: 'column'}}
          id="na-ship-viewer-tab"
          onSelect={this.handleSwitchTab}
          activeKey={activeTab}>
          <div>
            <div style={{marginBottom: 8}}>
              <Nav
                bsStyle="tabs"
                justified className="main-nav">
                <NavItem eventKey="info">
                  {__('ShipsTab.Info')}
                </NavItem>
                <NavItem eventKey="image">
                  {__('ShipsTab.Gallery')}
                </NavItem>
                {
                  !isAbyssalShip && (
                    <NavItem eventKey="voice">
                      {__('ShipsTab.Voice')}
                    </NavItem>
                  )
                }
              </Nav>
            </div>
            <div style={{flex: 1, height: 0, overflowY: 'auto'}}>
              <Tab.Content>
                <Tab.Pane eventKey="info">
                  {
                    isAbyssalShip ? (
                      <AbyssalInfoView
                        mstId={mstId}
                        shipGraphSource={shipGraphSource}
                        $ship={$ship}
                      />
                    ) : (
                      <ShipInfoView
                        mstId={mstId}
                        shipGraphSource={shipGraphSource}
                        $ship={$ship}
                      />
                    )
                  }
                </Tab.Pane>
                <Tab.Pane eventKey="image">
                  <GalleryView />
                </Tab.Pane>
                <Tab.Pane eventKey="voice">
                  <QuotesView />
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
  mergeMapStateToProps(
    shipViewerSelector,
    createStructuredSelector({
      shipGraphSources: shipGraphSourcesSelector,
      $ship: shipMasterDataSelector,
    })
  ),
  mapDispatchToProps,
)(ShipViewerImpl)

export { ShipViewer }
