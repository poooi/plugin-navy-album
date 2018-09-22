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
import {
  isMasterIdSpecialCGFuncSelector,
  getLastFetchFuncSelector,
} from '../../../selectors'
import { Header } from './header'
import { AltFormSwitcher } from './alt-form-switcher'
import { mapDispatchToProps } from '../../../store'
import { AbyssalInfoView } from './abyssal-info-view'
import { ShipInfoView } from './ship-info-view'
import { GalleryViewP2 } from './gallery-view-p2'
import { QuotesView } from './quotes-view'
import { ErrorBoundary } from '../../error-boundary'

class ShipViewerImpl extends Component {
  static propTypes = {
    style: PTyp.object.isRequired,
    activeTab: PTyp.string.isRequired,
    mstId: PTyp.number.isRequired,
    debuffFlag: PTyp.bool.isRequired,
    shipGraphSources: PTyp.object.isRequired,
    $ship: PTyp.object.isRequired,
    uiModify: PTyp.func.isRequired,
    isMasterIdSpecialCGFunc: PTyp.func.isRequired,
    lastFetch: PTyp.number.isRequired,
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
      style, activeTab, mstId, $ship, debuffFlag,
      shipGraphSources,
      isMasterIdSpecialCGFunc,
      lastFetch,
    } = this.props
    const {__} = window.i18n["poi-plugin-navy-album"]
    const isAbyssalShip = mstId > 1500
    const isSpecialCG = isMasterIdSpecialCGFunc(mstId)
    const shipGraphSource =
      _.get(shipGraphSources, isAbyssalShip ? 3 : 5, '')
    return (
      <Panel
        className="ship-viewer"
        style={style}
      >
        <Panel.Body>
          <ErrorBoundary>
            <Header isSpecialCG={isSpecialCG} />
            <AltFormSwitcher />
            {
              isSpecialCG ? (
                <GalleryViewP2
                  style={{flex: 1, height: 0, overflowY: 'auto'}}
                  mstId={mstId}
                  debuffFlag={debuffFlag}
                />
              ) : (
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
                      <Tab.Content animation={false}>
                        <Tab.Pane eventKey="info">
                          {
                            isAbyssalShip ? (
                              <AbyssalInfoView
                                mstId={mstId}
                                shipGraphSource={shipGraphSource}
                                lastFetch={lastFetch}
                                $ship={$ship}
                              />
                            ) : (
                              <ShipInfoView
                                mstId={mstId}
                                shipGraphSource={shipGraphSource}
                                lastFetch={lastFetch}
                                $ship={$ship}
                              />
                            )
                          }
                        </Tab.Pane>
                        <Tab.Pane eventKey="image">
                          <GalleryViewP2
                            style={{}}
                            debuffFlag={debuffFlag}
                            mstId={mstId}
                          />
                        </Tab.Pane>
                        <Tab.Pane eventKey="voice">
                          <QuotesView />
                        </Tab.Pane>
                      </Tab.Content>
                    </div>
                  </div>
                </Tab.Container>
              )
            }
          </ErrorBoundary>
        </Panel.Body>
      </Panel>
    )
  }
}

const ShipViewer = connect(
  mergeMapStateToProps(
    createStructuredSelector({
      shipGraphSources: shipGraphSourcesSelector,
      $ship: shipMasterDataSelector,
      isMasterIdSpecialCGFunc: isMasterIdSpecialCGFuncSelector,
    }),
    state => {
      const shipViewer = shipViewerSelector(state)
      const getLastFetch = getLastFetchFuncSelector(state)
      const lastFetch = getLastFetch(shipViewer.mstId)
      return {...shipViewer, lastFetch}
    }
  ),
  mapDispatchToProps,
)(ShipViewerImpl)

export { ShipViewer }
