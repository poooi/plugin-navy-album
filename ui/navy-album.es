import _ from 'lodash'
import React, { Component } from 'react'
import styled from 'styled-components'
import { Tab as BpTab, Tabs } from '@blueprintjs/core'
import { connect } from 'react-redux'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import { modifyObject } from 'subtender'

import { uiSelector, themeSelector } from '../selectors'
import { PTyp } from '../ptyp'
import { mapDispatchToProps } from '../store'
import { ShipsAlbum } from './ships-album'
import { EquipmentsAlbum } from './equipments-album'
import { MusicLibrary } from './music-library'
import { GameUpdateViewer } from './game-update-viewer'

const NaTabs = styled(Tabs)`
  flex: 1;
  display: flex;
  flex-direction: column;

  & .na-tab[role=tab] {
    flex: 1;
  }

  & .na-tab[role=tabpanel] {
    flex: 1;
    height: 0;
  }

`

@connect(
  createStructuredSelector({
    activeTab: createSelector(
      uiSelector,
      // TODO: figure out why the fork `ui` can be undefined.
      ui => _.get(ui, 'activeTab', 'ships')),
    theme: themeSelector,
  }),
  mapDispatchToProps,
)
class NavyAlbum extends Component {
  static propTypes = {
    activeTab: PTyp.ActiveTab.isRequired,
    theme: PTyp.string.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  handleSwitchTab = activeTab =>
    this.props.uiModify(
      modifyObject('activeTab', () => activeTab)
    )

  render() {
    const {activeTab, theme} = this.props
    const {__} = window.i18n["poi-plugin-navy-album"]
    return (
      <NaTabs
        id="na-main"
        className={`theme-${theme} na-main-tab`}
        selectedTabId={activeTab}
        onChange={this.handleSwitchTab}
        animate={false}
      >
        <BpTab
          className="na-tab"
          id="ships"
          title={__('Ships')}
          panel={<ShipsAlbum />}
        />
        <BpTab
          className="na-tab"
          id="equipments"
          title={__('Equipments')}
          panel={<EquipmentsAlbum />}
        />
        <BpTab
          className="na-tab"
          id="music"
          title={__('Music')}
          panel={<MusicLibrary />}
        />
        <BpTab
          className="na-tab"
          id="game-update"
          title={__('GameUpdate')}
          panel={<GameUpdateViewer />}
        />
      </NaTabs>
    )
  }
}

export { NavyAlbum }
