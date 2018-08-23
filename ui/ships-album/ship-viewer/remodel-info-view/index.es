import _ from 'lodash'
import { createStructuredSelector, createSelector } from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import {
  Panel, ListGroup,
} from 'react-bootstrap'
import {
  mergeMapStateToProps,
} from 'subtender'
import {
  constSelector,
} from 'views/utils/selectors'

import {
  remodelInfoSelector,
  remodelDetailsSelector,
} from '../../../../selectors'

import { PTyp } from '../../../../ptyp'
import { mapDispatchToProps } from '../../../../store'
import { InfoRow } from './info-row'

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
    const {__} = window.i18n["poi-plugin-navy-album"]
    return (
      <Panel
        className="remodel-info-view"
        style={{
          ...style,
        }}>
        <Panel.Heading>
          <div>{__('ShipsTab.Remodels')}</div>
        </Panel.Heading>
        <Panel.Body>
          <ListGroup>
            {
              currentRemodelDetails.map(detail => {
                const key = detail.mstIdBefore
                const props = {
                  style: {padding: '.4em .6em'},
                  mstId, mstIdToDesc, detail, key,
                }
                return (<InfoRow {...props} />)
              })
            }
          </ListGroup>
        </Panel.Body>
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
