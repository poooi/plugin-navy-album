import React, { PureComponent } from 'react'

import { PTyp } from '../../../ptyp'

class AbyssalInfoView extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipGraphSource: PTyp.string.isRequired,
  }
  render() {
    const {mstId, shipGraphSource} = this.props
    return (
      <img
        style={{maxWidth: '100%', height: 'auto'}}
        src={shipGraphSource}
        alt={`Data not yet available for ${mstId}`}
      />
    )
  }
}

export { AbyssalInfoView }
