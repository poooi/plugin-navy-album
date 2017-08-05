import _ from 'lodash'
import React, { Component } from 'react'
import { createStructuredSelector } from 'reselect'
import { connect } from 'react-redux'
import {
  Panel, Tab, Nav, NavItem,
  ListGroupItem, ListGroup,
} from 'react-bootstrap'
import { mergeMapStateToProps, modifyObject, generalComparator } from 'subtender'

import { PTyp } from '../../../ptyp'
import {
  shipViewerSelector,
  shipGraphPathSelector,
  shipGraphSourcesSelector,
  shipMasterDataSelector,
} from '../selectors'
import { Header } from './header'
import { AltFormSwitcher } from './alt-form-switcher'
import { mapDispatchToProps } from '../../../store'
import { AbyssalInfoView } from './abyssal-info-view'
import { ShipInfoView } from './ship-info-view'
import { QuotesView } from './quotes-view'


/*
   draft:

   # info viewer

   (following game UI)

   <RemodelChain>

   <ShipGraph> <StockEquipments>

   <Level Slider> // allow hp, asw, los, evs to be changed accordingly

   <ShipStats>  <ShipStatsExtra>

   // might need a separated viewer for abyssal ships
   - might try to fetch debuffed img if it's available
   - should be some other databases around about abyssal equipments

   # image viewer

   - perhaps: (1) centerize imgs, (2) allow exporting them.

   # voice player

   - ListGroup of <VoiceView>

   <VoiceView> should render play button, situation, and subtitles if it's available

 */

class ShipViewerImpl extends Component {
  static propTypes = {
    style: PTyp.object.isRequired,
    activeTab: PTyp.string.isRequired,
    mstId: PTyp.number.isRequired,
    shipGraphPath: PTyp.string,
    shipGraphSources: PTyp.object.isRequired,
    $ship: PTyp.object.isRequired,
    uiModify: PTyp.func.isRequired,
    requestSwf: PTyp.func.isRequired,
  }

  static defaultProps = {
    shipGraphPath: null,
  }

  requestShipGraph = (path = this.props.shipGraphPath) => {
    const {requestSwf} = this.props
    if (path) {
      requestSwf(path)
    }
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
    const characterIds =
      Object.keys(shipGraphSources).map(Number).sort(generalComparator)
    const shipGraphSource =
      _.get(shipGraphSources, mstId > 1500 ? 3 : 5, '')
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
                  Info
                </NavItem>
                <NavItem eventKey="image">
                  Image
                </NavItem>
                {
                  mstId <= 1500 && (
                    <NavItem eventKey="voice">
                      Voice
                    </NavItem>
                  )
                }
              </Nav>
            </div>
            <div style={{flex: 1, height: 0, overflowY: 'auto'}}>
              <Tab.Content>
                <Tab.Pane eventKey="info">
                  {
                    mstId > 1500 ? (
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
                  <ListGroup>
                    {
                      characterIds.map(chId => (
                        <ListGroupItem key={chId} style={{textAlign: 'center'}}>
                          <img
                            style={{maxWidth: '100%', height: 'auto'}}
                            src={_.get(shipGraphSources,chId,'')}
                            alt={`ship=${mstId}, chId=${chId}`}
                          />
                        </ListGroupItem>
                      ))
                    }
                  </ListGroup>
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
      shipGraphPath: shipGraphPathSelector,
      shipGraphSources: shipGraphSourcesSelector,
      $ship: shipMasterDataSelector,
    })
  ),
  mapDispatchToProps,
)(ShipViewerImpl)

export { ShipViewer }
