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

  componentDidMount() {
    this.requestShipGraph()
  }

  componentWillReceiveProps(nextProps) {
    // TODO: observer
    if (this.props.shipGraphPath !== nextProps.shipGraphPath) {
      this.requestShipGraph(nextProps.shipGraphPath)
    }
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
                    style={{
                      width: 218,
                      height: 300,
                    }}
                    src={_.get(shipGraphSources,5,'')}
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
