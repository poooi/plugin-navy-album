import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { PTyp } from '../ptyp'
import { mapDispatchToProps } from '../store'
import { shipGraphSourceFuncSelector } from '../selectors'

class ShipGraphViewImpl extends PureComponent {
  static propTypes = {
    // required props
    mstId: PTyp.number.isRequired,
    characterId: PTyp.number.isRequired,
    // optional props
    debuffFlag: PTyp.bool,
    style: PTyp.object,
    // connected
    src: PTyp.string.isRequired,
    requestShipGraph: PTyp.func.isRequired,
  }

  static defaultProps = {
    debuffFlag: false,
    style: {},
    src: '',
  }

  componentDidMount() {
    const {requestShipGraph, mstId} = this.props
    requestShipGraph(mstId)
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.mstId !== this.props.mstId) {
      nextProps.requestShipGraph(nextProps.mstId)
    }
  }

  render() {
    const {
      characterId: _ignored1,
      debuffFlag: _ignored2,
    } = this.props
    const {style,mstId,src} = this.props
    return (
      <img
        alt={`shipgraph-${mstId}`}
        src={src}
        style={style}
      />
    )
  }
}

const ShipGraphView = connect(
  (state, ownProps) => {
    const {mstId, characterId, debuffFlag} = ownProps
    const srcFunc = shipGraphSourceFuncSelector(state)
    return {
      src: srcFunc(mstId, characterId, debuffFlag),
    }
  },
  mapDispatchToProps
)(ShipGraphViewImpl)

export { ShipGraphView }
