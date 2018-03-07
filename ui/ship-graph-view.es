import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { PTyp } from '../ptyp'
import { mapDispatchToProps } from '../store'
import {
  shipGraphSourceFuncSelector,
  getLastFetchFuncSelector,
  swfCacheSelector,
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
    lastFetch: PTyp.number.isRequired,
    ready: PTyp.bool.isRequired,
    requestShipGraph: PTyp.func.isRequired,
  }

  static defaultProps = {
    debuffFlag: false,
    hideOnNoSrc: false,
    style: {},
  }

  componentDidMount() {
    const {requestShipGraph, mstId, ready} = this.props
    if (ready)
      requestShipGraph(mstId)
  }

  componentWillReceiveProps(nextProps) {
    if (
      nextProps.ready &&
      (
        !this.props.ready ||
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
      // https://stackoverflow.com/a/9943419
      lastFetch,
    } = this.props
    const {
      style,mstId,src,
      hideOnNoSrc,
    } = this.props
    return (
      <img
        alt={`shipgraph-${mstId}`}
        src={`${src}#${lastFetch}`}
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
    const getLastFetch = getLastFetchFuncSelector(state)
    const lastFetch = getLastFetch(mstId, debuffFlag)
    const {ready} = swfCacheSelector(state)
    return {
      src: srcFunc(mstId, characterId, debuffFlag),
      ready,
      lastFetch,
    }
  },
  mapDispatchToProps
)(ShipGraphViewImpl)

export { ShipGraphView }
