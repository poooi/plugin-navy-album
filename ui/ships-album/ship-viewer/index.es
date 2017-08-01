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
} from '../selectors'
import { Header } from './header'
import { mapDispatchToProps } from '../../../store'


/*
   draft:

   # info viewer

   (following game UI)

   <RemodelChain>

   <StockEquipments>     <ShipGraph>

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
      style, activeTab, mstId,
      shipGraphSources,
    } = this.props
    const characterIds =
      Object.keys(shipGraphSources).map(Number).sort(generalComparator)
    return (
      <Panel
        className="ship-viewer"
        style={style}
      >
        <Header />
        <Tab.Container
          id="na-ship-viewer-tab"
          onSelect={this.handleSwitchTab}
          style={{flex: 1, overflowY: 'scroll'}}
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
            <div style={{flex: 1}}>
              <Tab.Content>
                <Tab.Pane eventKey="info">
                  <img
                    style={
                      mstId > 1500 ? {
                        width: '100%', height: 'auto',
                      } : {
                        width: 218, height: 300,
                      }
                    }
                    src={_.get(shipGraphSources, mstId > 1500 ? 3 : 5, '')}
                    alt={`Data not yet available for ${mstId}`}
                  />
                </Tab.Pane>
                <Tab.Pane eventKey="image">
                  <ListGroup>
                    {
                      characterIds.map(chId => (
                        <ListGroupItem key={chId}>
                          <img
                            src={_.get(shipGraphSources,chId,'')}
                            alt={`ship=${mstId}, chId=${chId}`}
                          />
                        </ListGroupItem>
                      ))
                    }
                  </ListGroup>
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
  mergeMapStateToProps(
    shipViewerSelector,
    createStructuredSelector({
      shipGraphPath: shipGraphPathSelector,
      shipGraphSources: shipGraphSourcesSelector,
    })
  ),
  mapDispatchToProps,
)(ShipViewerImpl)

export { ShipViewer }
