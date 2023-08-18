import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { Tooltip } from 'views/components/etc/overlay'

import { mstIdToCategoryFuncSelector } from './selectors'
import { PTyp } from '../../ptyp'
import { ShipGraphView } from '../ship-graph-view'

/*
   ShipGraphView with CG in tooltip (if available)
 */
@connect(
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
)
class ShipGraphViewWithCG extends PureComponent {
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
      mstId, graphAttrs,
      prefix,
    } = this.props
    const uiKey = `${prefix}ship-cg-${mstId}`
    const cgAvailable = graphAttrs.length > 0
    const contentProps = {
      style: graphSize,
      mstId,
      debuffFlag: false,
      graphType: 'banner',
    }

    if (cgAvailable) {
      return (
        <Tooltip
          key={uiKey}
          content={
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
          }
          placement="bottom"
        >
          <ShipGraphView
            {...contentProps}
          />
        </Tooltip>
      )
    } else {
      return (
        <ShipGraphView
          key={uiKey}
          {...contentProps}
        />
      )
    }
  }
}

export { ShipGraphViewWithCG }
