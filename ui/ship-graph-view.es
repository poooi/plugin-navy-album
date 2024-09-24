import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { PTyp } from '../ptyp'
import { mapDispatchToProps } from '../store'
import {
  getShipImgSrcFuncSelector,
} from '../selectors'

@connect(
  (state, ownProps) => {
    const {mstId, graphType, damaged, debuffFlag} = ownProps
    const getShipImgSrc = getShipImgSrcFuncSelector(state)
    return {
      src: getShipImgSrc(mstId, graphType, damaged, debuffFlag),
    }
  },
  mapDispatchToProps
)
class ShipGraphView extends PureComponent {
  static propTypes = {
    // required props
    mstId: PTyp.number.isRequired,
    graphType: PTyp.string.isRequired,
    damaged: PTyp.bool.isRequired,
    debuffFlag: PTyp.bool.isRequired,

    // optional props
    style: PTyp.object,
    // whether the component hides itself
    // when no source is available
    hideOnNoSrc: PTyp.bool,
    // connected
    src: PTyp.string.isRequired,
  }

  static defaultProps = {
    hideOnNoSrc: false,
    style: {},
  }

  render() {
    const {
      style,mstId,src,
      hideOnNoSrc,

      // used in selectors
      graphType: _ignored1,
      damaged: _ignored2,
      debuffFlag: _ignored3,
    } = this.props
    return (
      <img
        alt={`shipgraph-${mstId}`}
        src={`${src}`}
        style={{
          ...style,
          ...(hideOnNoSrc && !src ? {display: 'none'} : {}),
        }}
      />
    )
  }
}

export { ShipGraphView }
