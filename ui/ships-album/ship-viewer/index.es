import React, { Component } from 'react'
import { createStructuredSelector } from 'reselect'
import { connect } from 'react-redux'
import styled from 'styled-components'
import { Tab, Tabs } from '@blueprintjs/core'
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

const SvTabs = styled(Tabs)`
  flex: 1;
  height: 0;
  display: flex;
  flex-direction: column;

  & .ships-tab {
    flex: 1;
    overflow-y: auto;
  }
`

@connect(
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
)
class ShipViewer extends Component {
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

  renderFriendlyOrAbyssal() {
    const {
      activeTab, mstId, $ship, debuffFlag,
    } = this.props
    const {__} = window.i18n["poi-plugin-navy-album"]
    const isAbyssalShip = isAbyssalShipMstId(mstId)
    return (
      <SvTabs
        id="ship-viewer"
        selectedTabId={activeTab}
        onChange={this.handleSwitchTab}
        animate={false}
      >
        <Tab
          className="ships-tab"
          id="info"
          title={__('ShipsTab.Info')}
          panel={
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
        />
        <Tab
          className="ships-tab"
          id="image"
          title={__('ShipsTab.Gallery')}
          panel={
            <GalleryView
              style={{}}
              debuffFlag={debuffFlag}
              mstId={mstId}
            />
          }
        />
        {
          !isAbyssalShip && (
            <Tab
              className="ships-tab"
              id="voice"
              title={__('ShipsTab.Voice')}
              panel={<QuotesView />}
            />
          )
        }
      </SvTabs>
    )
  }

  render() {
    const {
      style, mstId, debuffFlag,
      isMasterIdSpecialCGFunc,
    } = this.props
    const isSpecialCG = isMasterIdSpecialCGFunc(mstId)
    return (
      <div
        className="ship-viewer"
        style={{
          display: 'flex',
          flexDirection: 'column',
          ...style,
        }}
      >
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
            ) : this.renderFriendlyOrAbyssal()
          }
        </ErrorBoundary>
      </div>
    )
  }
}

export { ShipViewer }
