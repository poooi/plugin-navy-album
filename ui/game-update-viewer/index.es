import _ from 'lodash'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import { mergeMapStateToProps } from 'subtender'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import {
  Panel, Button,
} from 'react-bootstrap'
import { constSelector } from 'views/utils/selectors'
import { SlotitemIcon } from 'views/components/etc/icon'

import {
  gameUpdateSelector,
  serverIpSelector,
} from '../../selectors'
import { PTyp } from '../../ptyp'
import { ShipGraphView } from '../ship-graph-view'
import { mapDispatchToProps } from '../../store'

const isAbyssalMstId = mstId => mstId > 1500
const isAbyssalEquipMstId = eMstId => eMstId > 500

const renderShipGraphRow = (mstIds, rowKey, uiSwitchShip) =>
  mstIds.length > 0 && (
    <div
      style={{display: 'flex', flexWrap: 'wrap', marginBottom: '1em'}}
      key={rowKey}>
      {
        mstIds.map(mstId => (
          <div
            style={{
              margin: 4,
              cursor: 'pointer',
            }}
            onClick={() => uiSwitchShip(mstId)}
            key={mstId}>
            <ShipGraphView
              style={{width: 160, height: 40}}
              mstId={mstId}
              characterId={1}
            />
          </div>
        ))
      }
    </div>
  )

class GameUpdateViewerImpl extends PureComponent {
  static propTypes = {
    digest: PTyp.object,
    summary: PTyp.object,
    serverIp: PTyp.string.isRequired,
    $equips: PTyp.object.isRequired,
    uiSwitchShip: PTyp.func.isRequired,
    uiSwitchEquip: PTyp.func.isRequired,
  }

  static defaultProps = {
    digest: null,
    summary: null,
  }

  renderNewShipsPart = () => {
    const {summary, uiSwitchShip} = this.props
    const [abyssalMstIds, friendlyMstIds] =
      _.partition(summary.addedShipMstIds,isAbyssalMstId)
    return summary.addedShipMstIds.length > 0 && [
      <h3 key="sh-1">New Ships</h3>,
      renderShipGraphRow(friendlyMstIds,"sh-2",uiSwitchShip),
      renderShipGraphRow(abyssalMstIds,"sh-3",uiSwitchShip),
    ]
  }

  renderNewEquipsPart = () => {
    const {serverIp, summary, $equips, uiSwitchEquip} = this.props
    const equipMstIdToSrc = mstId => {
      const mstIdStr = String(mstId).padStart(3,'0')
      const prefix = `http://${serverIp}/kcs/resources/image/slotitem/`
      return `${prefix}card/${mstIdStr}.png`
    }
    const [abyssalEqMstIds, friendlyEqMstIds] =
      _.partition(summary.addedEquipMstIds,isAbyssalEquipMstId)

    const mkSmallEquip = mstId => {
      const $equip = $equips[mstId]
      const iconId = $equip.api_type[3]
      return (
        <Button
          onClick={() => uiSwitchEquip(mstId)}
          style={{
            fontSize: '1.4em',
            margin: 4,
          }}
          key={mstId}>
          <div style={{
            display: 'flex', alignItems: 'center',
          }}>
            <SlotitemIcon
              className="slotitem-img"
              slotitemId={iconId}
            />
            <span>{$equip.api_name}</span>
          </div>
        </Button>
      )
    }

    return summary.addedEquipMstIds.length > 0 && [
      <h3 key="eq-1">New Equipments</h3>,
      friendlyEqMstIds.length > 0 && (
        <div
          style={{display: 'flex', flexWrap: 'wrap', marginBottom: '1em'}}
          key="eq-2">
          {
            friendlyEqMstIds.map(mstId => (
              <div
                key={mstId}
                onClick={() => uiSwitchEquip(mstId)}
                style={{
                  margin: 4,
                  cursor: 'pointer',
                }}>
                <img
                  alt={`eqp-${mstId}`}
                  style={{
                    width: 128,
                    height: 128,
                  }}
                  src={equipMstIdToSrc(mstId)}
                />
              </div>
            ))
          }
        </div>
      ),
      abyssalEqMstIds.length > 0 && (
        <div
          style={{display: 'flex', flexWrap: 'wrap', marginBottom: '1em'}}
          key="eq-3">
          {
            abyssalEqMstIds.map(mkSmallEquip)
          }
        </div>
      ),
    ]
  }

  renderNewCGsPart = () => {
    const {summary, uiSwitchShip} = this.props
    const [abyssalMstIds, friendlyMstIds] =
      _.partition(summary.changedShipMstIds,isAbyssalMstId)
    return summary.changedShipMstIds.length > 0 && [
      <h3 key="cg-1">New CGs</h3>,
      renderShipGraphRow(friendlyMstIds,"cg-2",uiSwitchShip),
      renderShipGraphRow(abyssalMstIds,"cg-3",uiSwitchShip),
    ]
  }

  renderGeneralInfoPart = () => {
    const {digest} = this.props
    return [
      <h3 key="gi-1">General Info</h3>,
      <p key="gi-2">
        {
          [
            `Ship Count:`,
            `${digest.shipDigests.length},`,
            `Equipment Count:`,
            `${digest.equipMstIds.length}`,
          ].join(' ')
        }
      </p>,
    ]
  }

  render() {
    const {digest, summary} = this.props
    return (
      <div style={{height: '100%', display: 'flex', flexDirection: 'column'}}>
        <Panel className="game-update-viewer" style={{marginBottom: 8, flex: 1}}>
          <div style={{overflowY: 'auto', height: 0, flex: 1}}>
            {
              summary && _.concat(
                this.renderNewShipsPart(),
                this.renderNewEquipsPart(),
                this.renderNewCGsPart()
              )
            }
            {digest && this.renderGeneralInfoPart()}
          </div>
        </Panel>
      </div>
    )
  }
}

const GameUpdateViewer = connect(
  mergeMapStateToProps(
    gameUpdateSelector,
    createStructuredSelector({
      serverIp: serverIpSelector,
      $equips: createSelector(
        constSelector,
        ({$equips}) => $equips
      ),
    })
  ),
  mapDispatchToProps,
)(GameUpdateViewerImpl)

export { GameUpdateViewer }
