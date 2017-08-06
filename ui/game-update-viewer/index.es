import _ from 'lodash'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import { mergeMapStateToProps } from 'subtender'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import {
  Panel, Label, Button,
} from 'react-bootstrap'
import { constSelector } from 'views/utils/selectors'
import { SlotitemIcon } from 'views/components/etc/icon'

import {
  gameUpdateSelector,
  serverIpSelector,
} from '../../selectors'
import { PTyp } from '../../ptyp'
import { ShipGraphView } from '../ship-graph-view'

const isAbyssalMstId = mstId => mstId > 1500
const isAbyssalEquipMstId = eMstId => eMstId > 500

const renderShipGraphRow = (mstIds, rowKey) =>
  mstIds.length > 0 && (
    <p key={rowKey}>
      {
        mstIds.map(mstId => (
          <ShipGraphView
            style={{width: 160, height: 40, margin: 4}}
            key={mstId}
            mstId={mstId}
            characterId={1}
          />
        ))
      }
    </p>
  )

class GameUpdateViewerImpl extends PureComponent {
  static propTypes = {
    digest: PTyp.object,
    summary: PTyp.object,
    serverIp: PTyp.string.isRequired,
    $equips: PTyp.object.isRequired,
  }

  static defaultProps = {
    digest: null,
    summary: null,
  }

  renderNewShipsPart = () => {
    const {summary} = this.props
    const [abyssalMstIds, friendlyMstIds] =
      _.partition(summary.addedShipMstIds,isAbyssalMstId)
    return summary.addedShipMstIds.length > 0 && [
      <h3 key="sh-1">New Ships</h3>,
      renderShipGraphRow(friendlyMstIds,"sh-2"),
      renderShipGraphRow(abyssalMstIds,"sh-3"),
    ]
  }

  renderNewEquipsPart = () => {
    const {serverIp, summary, $equips} = this.props
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
        <p key="eq-2">
          {
            friendlyEqMstIds.map(mstId => (
              <img
                key={mstId}
                alt={`eqp-${mstId}`}
                style={{
                  width: 128,
                  height: 128,
                  margin: 4,
                }}
                src={equipMstIdToSrc(mstId)}
              />
            ))
          }
        </p>
      ),
      abyssalEqMstIds.length > 0 && (
        <p key="eq-3">
          {
            abyssalEqMstIds.map(mkSmallEquip)
          }
        </p>
      ),
    ]
  }

  renderNewCGsPart = () => {
    const {summary} = this.props
    const [abyssalMstIds, friendlyMstIds] =
      _.partition(summary.changedShipMstIds,isAbyssalMstId)
    return summary.changedShipMstIds.length > 0 && [
      <h3 key="cg-1">New CGs</h3>,
      renderShipGraphRow(friendlyMstIds,"cg-2"),
      renderShipGraphRow(abyssalMstIds,"cg-3"),
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
              summary && [
                ...this.renderNewShipsPart(),
                ...this.renderNewEquipsPart(),
                ...this.renderNewCGsPart(),
              ]
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
  )
)(GameUpdateViewerImpl)

export { GameUpdateViewer }
