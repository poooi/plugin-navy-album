import React, { PureComponent } from 'react'
import { connect } from 'react-redux'

class MapBgmViewerImpl extends PureComponent {
  render() {
    return (
      <div>MapBgmViewer</div>
    )
  }
}

const MapBgmViewer = connect(
)(MapBgmViewerImpl)

export {
  MapBgmViewer,
}
