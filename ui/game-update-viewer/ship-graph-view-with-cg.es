import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { OverlayTrigger, Tooltip } from 'react-bootstrap'

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
    graphAttrs: PTyp.array.isRequired,
  }

  render() {
    const {
      graphSize,
      mstId, graphAttrs, prefix,
    } = this.props
    const cgAvailable = graphAttrs.length > 0
    const content = (
      <ShipGraphView
        style={graphSize}
        mstId={mstId}
        debuffFlag={false}
        graphType="banner"
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
                  graphAttrs.map(({graphType, damaged}) => (
                    <ShipGraphView
                      key={graphType}
                      style={{maxHeight: 400, maxWidth: 400}}
                      mstId={mstId}
                      graphType={graphType}
                      damaged={damaged}
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
    let graphAttrs
    if (cat === 'friendly') {
      graphAttrs = [
        {graphType: 'full', damaged: false},
        {graphType: 'full', damaged: true},
      ]
    } else if (cat === 'abyssal') {
      graphAttrs = [{graphType: 'full', damaged: false}]
    } else if (cat === 'special') {
      // no character id for special for now
      graphAttrs = [
        {graphType: 'character_full', damaged: false},
        {graphType: 'character_full', damaged: true},
      ]
    }
    return {graphAttrs}
  },
)(ShipGraphViewWithCGImpl)

export { ShipGraphViewWithCG }
