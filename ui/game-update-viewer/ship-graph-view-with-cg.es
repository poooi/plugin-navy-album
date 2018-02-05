import _ from 'lodash'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { OverlayTrigger, Tooltip } from 'react-bootstrap'

import { swfCacheSelector } from '../../selectors'
import { mstIdToCategoryFuncSelector } from './selectors'
import { PTyp } from '../../ptyp'
import { ShipGraphView } from '../ship-graph-view'

/*
   ShipGraphView with CG in tooltip (if available)
 */

class ShipGraphViewWithCGImpl extends PureComponent {
  static propTypes = {
    graphSize: PTyp.shape({
      width: PTyp.number.isRequired,
      height: PTyp.number.isRequired,
    }).isRequired,
    mstId: PTyp.number.isRequired,
    prefix: PTyp.string.isRequired,

    // connected:
    chIds: PTyp.array.isRequired,
  }

  render() {
    const {
      graphSize,
      mstId, chIds, prefix,
    } = this.props
    const cgAvailable = chIds.length > 0
    const content = (
      <ShipGraphView
        style={graphSize}
        mstId={mstId}
        characterId={1}
      />
    )

    if (cgAvailable) {
      return (
        <OverlayTrigger
          placement="bottom"
          overlay={
            <Tooltip
              id={`${prefix}ship-cg-${mstId}`}
              className="game-update-shipcg-tooltip"
            >
              <div style={{display: 'flex', alignItems: 'center'}}>
                {
                  chIds.map(chId => (
                    <ShipGraphView
                      key={chId}
                      style={{maxHeight: 400, maxWidth: 400}}
                      mstId={mstId}
                      characterId={chId}
                    />
                  ))
                }
              </div>
            </Tooltip>
          }
        >
          <div>
            {content}
          </div>
        </OverlayTrigger>
      )
    } else {
      return content
    }
  }
}

const ShipGraphViewWithCG = connect(
  (state, ownProps) => {
    const {mstId} = ownProps
    const cat = mstIdToCategoryFuncSelector(state)(mstId)
    const swfCache = swfCacheSelector(state)
    let chIds = []
    const checkAvailable = chId =>
      _.get(swfCache, ['ship', mstId, 'files', chId])
    if (cat === 'friendly') {
      chIds = [17,19].filter(checkAvailable)
    } else if (cat === 'abyssal') {
      chIds = [3].filter(checkAvailable)
    // no character id for special for now
    }
    return {chIds}
  },
)(ShipGraphViewWithCGImpl)

export { ShipGraphViewWithCG }
