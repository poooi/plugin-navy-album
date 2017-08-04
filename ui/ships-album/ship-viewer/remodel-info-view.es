import _ from 'lodash'
import { createStructuredSelector, createSelector } from 'reselect'
import React, { PureComponent } from 'react'
import FontAwesome from 'react-fontawesome'
import { connect } from 'react-redux'
import {
  Panel, ListGroup, ListGroupItem,
} from 'react-bootstrap'
import {
  mergeMapStateToProps,
} from 'subtender'
import {
  constSelector,
} from 'views/utils/selectors'
import { MaterialIcon } from 'views/components/etc/icon'

import {
  remodelInfoSelector,
  remodelDetailsSelector,
} from '../../../selectors'

import { PTyp } from '../../../ptyp'
import { mapDispatchToProps } from '../../../store'
import { Icon } from './icon'

const prepareMstIdToDesc = ($ships, $shipTypes) => mstId => {
  const $ship = $ships[mstId]
  return {
    shipName: $ship.api_name,
    typeName: $shipTypes[$ship.api_stype].api_name,
  }
}

class RemodelInfoViewImpl extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    mstId: PTyp.number.isRequired,
    remodelDetails: PTyp.object.isRequired,
    remodelInfo: PTyp.object.isRequired,
    $ships: PTyp.object.isRequired,
    $shipTypes: PTyp.object.isRequired,
    uiSwitchShip: PTyp.func.isRequired,
  }

  render() {
    const {
      style,
      mstId,
      remodelInfo, remodelDetails,
      $ships, $shipTypes,
    } = this.props
    const noRender = (<div style={{display: 'none'}} />)
    const {originMstIdOf, remodelChains} = remodelInfo
    const originMstId = originMstIdOf[mstId]
    if (! originMstId)
      return noRender
    const remodelChain = remodelChains[originMstId]
    if (! remodelChain)
      return noRender
    const currentRemodelDetails = _.flatMap(
      remodelChain,
      curMstId => {
        const detail = remodelDetails[curMstId]
        return detail ? [detail] : []
      })
    if (currentRemodelDetails.length === 0)
      return noRender
    const mstIdToDesc = prepareMstIdToDesc($ships, $shipTypes)
    const mkShipView = curMstId => {
      const {shipName, typeName} = mstIdToDesc(curMstId)
      const className = curMstId === mstId ? 'text-primary' : ''
      return (
        <div>
          <div className={className}>{typeName}</div>
          <div
            className={className}
            style={{fontSize: '1.6em', marginLeft: '.3em'}}>
            {shipName}
          </div>
        </div>
      )
    }

    return (
      <Panel
        className="remodel-info-view"
        header={<div>Remodels</div>}
        style={{
          ...style,
        }}>
        <ListGroup fill>
          {
            currentRemodelDetails.map(detail => {
              const key = detail.mstIdBefore
              return (
                <ListGroupItem
                  style={{
                    padding: '.4em .6em',
                  }}
                  key={key}>
                  <div
                    style={{
                      display: 'flex',
                      alignItems: 'center',

                    }}
                  >
                    <span style={{width: '3em', fontSize: '1.6em'}}>
                      Lv. {detail.level}
                    </span>
                    {mkShipView(detail.mstIdBefore)}
                    <FontAwesome
                      name="arrow-right"
                      style={{
                        margin: '.2em .8em',
                      }}
                    />
                    {mkShipView(detail.mstIdAfter)}
                  </div>
                  <div style={{display: 'flex', alignItems: 'center'}}>
                    {
                      _.flatMap(
                        'ammo steel devMat instantBuild blueprint catapult'
                          .split(' '),
                        itemOrResource => {
                          const matIds = {
                            ammo: 2,
                            steel: 3,
                            devMat: 7,
                            instantBuild: 5,
                          }
                          const v = detail[itemOrResource]
                          if (v === 0)
                            return []
                          const icon = matIds[itemOrResource] ? (
                            <MaterialIcon
                              materialId={matIds[itemOrResource]}
                              className="material-icon"
                            />
                          ) : (
                            <Icon
                              style={{width: '1.5em', height: '1.5em'}}
                              name={itemOrResource} />
                          )
                          const text =
                            ['ammo', 'steel'].includes(itemOrResource) ?
                              String(v) :
                              `x${v}`
                          return [
                            <span
                              key={itemOrResource}
                              style={{
                                display: 'flex',
                                alignItems: 'center',
                                marginRight: '.5em',
                              }}>
                              {icon}
                              <span>
                                {text}
                              </span>
                            </span>,
                          ]
                        }
                      )
                    }
                  </div>
                </ListGroupItem>
              )
            })
          }
        </ListGroup>
      </Panel>
    )
  }
}

const RemodelInfoView = connect(
  mergeMapStateToProps(
    createStructuredSelector({
      remodelDetails: remodelDetailsSelector,
      remodelInfo: remodelInfoSelector,
    }),
    createSelector(
      constSelector,
      ({$ships,$shipTypes}) => ({$ships,$shipTypes})
    )
  ),
  mapDispatchToProps,
)(RemodelInfoViewImpl)

export { RemodelInfoView }
