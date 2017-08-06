import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { PTyp } from '../ptyp'
import { mapDispatchToProps } from '../store'
import {
  shipGraphSourceFuncSelector,
  swfDatabaseSelector,
} from '../selectors'

class ShipGraphViewImpl extends PureComponent {
  static propTypes = {
    // required props
    mstId: PTyp.number.isRequired,
    characterId: PTyp.number.isRequired,
    // optional props
    debuffFlag: PTyp.bool,
    style: PTyp.object,
    // whether the component hides itself
    // when no source is available
    hideOnNoSrc: PTyp.bool,
    // connected
    src: PTyp.string.isRequired,
    diskFilesReady: PTyp.bool.isRequired,
    requestShipGraph: PTyp.func.isRequired,
  }

  static defaultProps = {
    debuffFlag: false,
    hideOnNoSrc: false,
    style: {},
    src: '',
  }

  componentDidMount() {
    const {requestShipGraph, mstId, diskFilesReady} = this.props
    if (diskFilesReady)
      requestShipGraph(mstId)
  }

  componentWillReceiveProps(nextProps) {
    if (
      nextProps.diskFilesReady &&
      (
        !this.props.diskFilesReady ||
        nextProps.mstId !== this.props.mstId
      )
    ) {
      nextProps.requestShipGraph(nextProps.mstId)
    }
  }

  render() {
    const {
      characterId: _ignored1,
      debuffFlag: _ignored2,
    } = this.props
    const {
      style,mstId,src,
      hideOnNoSrc,
    } = this.props
    return (
      <img
        alt={`shipgraph-${mstId}`}
        src={src}
        style={{
          ...style,
          ...(hideOnNoSrc && !src ? {display: 'none'} : {}),
        }}
      />
    )
  }
}

const ShipGraphView = connect(
  (state, ownProps) => {
    const {mstId, characterId, debuffFlag} = ownProps
    const srcFunc = shipGraphSourceFuncSelector(state)
    const {diskFilesReady} = swfDatabaseSelector(state)
    return {
      src: srcFunc(mstId, characterId, debuffFlag),
      diskFilesReady,
    }
  },
  mapDispatchToProps
)(ShipGraphViewImpl)

export { ShipGraphView }
