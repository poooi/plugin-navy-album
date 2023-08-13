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
  shipMasterDataSelector,
} from '../selectors'
import {
  isMasterIdSpecialCGFuncSelector,
} from '../../../selectors'
import {
  isAbyssalShipMstId,
} from '../../../game-misc'
import { Header } from './header'
import { AltFormSwitcher } from './alt-form-switcher'
import { mapDispatchToProps } from '../../../store'
import { AbyssalInfoView } from './abyssal-info-view'
import { ShipInfoView } from './ship-info-view'
import { GalleryView } from './gallery-view'
import { QuotesView } from './quotes-view'
import { ErrorBoundary } from '../../error-boundary'

class ShipViewerImpl extends Component {
  static propTypes = {
    style: PTyp.object.isRequired,
    activeTab: PTyp.string.isRequired,
    mstId: PTyp.number.isRequired,
    debuffFlag: PTyp.bool.isRequired,
    $ship: PTyp.object.isRequired,
    uiModify: PTyp.func.isRequired,
    isMasterIdSpecialCGFunc: PTyp.func.isRequired,
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
      isMasterIdSpecialCGFunc,
    } = this.props
    const {__} = window.i18n["poi-plugin-navy-album"]
    const isAbyssalShip = isAbyssalShipMstId(mstId)
    const isSpecialCG = isMasterIdSpecialCGFunc(mstId)
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
                <GalleryView
                  style={{flex: 1, height: 0, overflowY: 'auto'}}
                  mstId={mstId}
                  debuffFlag={debuffFlag}
                />
              ) : (
                <Tab.Container
                  style={{
                    flex: 1,
                    display: 'flex',
                    flexDirection: 'column',
                    height: 0,
                  }}
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
                                debuffFlag={debuffFlag}
                                $ship={$ship}
                              />
                            ) : (
                              <ShipInfoView
                                mstId={mstId}
                                $ship={$ship}
                              />
                            )
                          }
                        </Tab.Pane>
                        <Tab.Pane eventKey="image">
                          <GalleryView
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
      $ship: shipMasterDataSelector,
      isMasterIdSpecialCGFunc: isMasterIdSpecialCGFuncSelector,
    }),
    state => {
      const shipViewer = shipViewerSelector(state)
      return {...shipViewer}
    }
  ),
  mapDispatchToProps,
)(ShipViewerImpl)

export { ShipViewer }
