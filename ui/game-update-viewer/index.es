import _ from 'lodash'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import { mergeMapStateToProps } from 'subtender'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { Button } from '@blueprintjs/core'
import styled from 'styled-components'

import { constSelector } from 'views/utils/selectors'
import { SlotitemIcon } from 'views/components/etc/icon'

import {
  gameUpdateSelector,
  serverIpSelector,
} from '../../selectors'
import {
  reorganizedSummarySelector,
} from './selectors'
import { PTyp } from '../../ptyp'
import { ErrorBoundary } from '../error-boundary'
import { ShipGraphViewWithCG } from './ship-graph-view-with-cg'
import { getEquipImgPath } from '../../game-misc'
import { mapDispatchToProps } from '../../store'

const renderShipGraphRow = (
  mstIds, rowKey, uiSwitchShip, prefix = 'gameupdate-default-prefix-',
  graphSize = {width: 160, height: 40}
) =>
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
            <ShipGraphViewWithCG
              graphSize={graphSize}
              mstId={mstId}
              prefix={prefix}
            />
          </div>
        ))
      }
    </div>
  )

const GrouppedShipMstIds = PTyp.shape({
  special: PTyp.array.isRequired,
  friendly: PTyp.array.isRequired,
  abyssal: PTyp.array.isRequired,
})

const GrouppedEquipMstIds = PTyp.shape({
  friendly: PTyp.array.isRequired,
  abyssal: PTyp.array.isRequired,
})

const EqpIcon = styled(SlotitemIcon)`
  height: 1.4em;
  width: 1.4em;
`

@connect(
  mergeMapStateToProps(
    state => {
      const {summary, digest} = gameUpdateSelector(state)
      return {
        summaryAvailable: !_.isEmpty(summary),
        digest,
      }
    },
    createStructuredSelector({
      rSummary: reorganizedSummarySelector,
      serverIp: serverIpSelector,
      $equips: createSelector(
        constSelector,
        ({$equips}) => $equips
      ),
    })
  ),
  mapDispatchToProps,
)
class GameUpdateViewer extends PureComponent {
  static propTypes = {
    summaryAvailable: PTyp.bool.isRequired,
    digest: PTyp.object,
    rSummary: PTyp.shape({
      addedShipMstIds: GrouppedShipMstIds.isRequired,
      addedEquipMstIds: GrouppedEquipMstIds.isRequired,
      changedShipMstIds: GrouppedShipMstIds.isRequired,
    }).isRequired,
    serverIp: PTyp.string.isRequired,
    $equips: PTyp.object.isRequired,
    uiSwitchShip: PTyp.func.isRequired,
    uiSwitchEquip: PTyp.func.isRequired,
  }

  static defaultProps = {
    digest: null,
  }

  renderNewShipsPart = () => {
    const {__} = window.i18n["poi-plugin-navy-album"]
    const {rSummary: {addedShipMstIds}, uiSwitchShip} = this.props
    const {special, friendly, abyssal} = addedShipMstIds
    const length = _.sum(_.values(addedShipMstIds).map(x => x.length))
    const prefix = 'gameupdate-newship-'
    return length > 0 && [
      <h3 key="sh-1">{__('GameUpdateTab.NewShips')}</h3>,
      renderShipGraphRow(friendly,"sh-2",uiSwitchShip,prefix),
      renderShipGraphRow(
        special,"sh-3",uiSwitchShip,prefix,
        {width: 218, height: 300}
      ),
      renderShipGraphRow(abyssal,"sh-4",uiSwitchShip,prefix),
    ]
  }

  renderNewEquipsPart = () => {
    const {serverIp, rSummary: {addedEquipMstIds}, $equips, uiSwitchEquip} = this.props
    const {friendly, abyssal} = addedEquipMstIds
    const length = _.sum(_.values(addedEquipMstIds).map(x => x.length))
    const equipMstIdToSrc = mstId =>
      `http://${serverIp}${getEquipImgPath(mstId, 'card')}`

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
            <EqpIcon
              className="slotitem-img"
              slotitemId={iconId}
            />
            <span>{$equip.api_name}</span>
          </div>
        </Button>
      )
    }
    const {__} = window.i18n["poi-plugin-navy-album"]
    return length > 0 && [
      <h3 key="eq-1">{__('GameUpdateTab.NewEquipments')}</h3>,
      friendly.length > 0 && (
        <div
          style={{display: 'flex', flexWrap: 'wrap', marginBottom: '1em'}}
          key="eq-2">
          {
            friendly.map(mstId => (
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
      abyssal.length > 0 && (
        <div
          style={{display: 'flex', flexWrap: 'wrap', marginBottom: '1em'}}
          key="eq-3">
          {
            abyssal.map(mkSmallEquip)
          }
        </div>
      ),
    ]
  }

  renderUpdatedCGsPart = () => {
    const {__} = window.i18n["poi-plugin-navy-album"]
    const {rSummary: {changedShipMstIds}, uiSwitchShip} = this.props
    const {special, friendly, abyssal} = changedShipMstIds
    // friendly.push(973)
    // abyssal.push(1708)
    const length = _.sum(_.values(changedShipMstIds).map(x => x.length))
    const prefix = 'gameupdate-changedship-'
    return length > 0 && [
      <h3 key="cg-1">{__('GameUpdateTab.UpdatedCGs')}</h3>,
      renderShipGraphRow(friendly,"cg-2",uiSwitchShip, prefix),
      renderShipGraphRow(
        special,"cg-3",uiSwitchShip,prefix,
        {width: 218, height: 300}),
      renderShipGraphRow(abyssal,"cg-4",uiSwitchShip, prefix),
    ]
  }

  renderGeneralInfoPart = () => {
    const {digest} = this.props
    const {__} = window.i18n["poi-plugin-navy-album"]
    return [
      <h3 key="gi-1">{__('GameUpdateTab.GeneralInfo')}</h3>,
      <p key="gi-2">
        {
          __(
            'GameUpdateTab.GeneralInfoDetail',
            digest.shipDigests.length,
            digest.equipMstIds.length,
          )
        }
      </p>,
    ]
  }

  render() {
    const {digest, summaryAvailable} = this.props
    return (
      <div style={{height: '100%', display: 'flex', flexDirection: 'column'}}>
        <ErrorBoundary>
          <div
            style={{
              marginBottom: 8,
              flex: 1,
              height: '100%',
              overflowY: 'auto',
            }}>
            {
              summaryAvailable && _.concat(
                this.renderNewShipsPart(),
                this.renderNewEquipsPart(),
                this.renderUpdatedCGsPart()
              )
            }
            {digest && this.renderGeneralInfoPart()}
          </div>
        </ErrorBoundary>
      </div>
    )
  }
}

export { GameUpdateViewer }
