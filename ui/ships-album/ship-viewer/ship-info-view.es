import React, { PureComponent } from 'react'

import { PTyp } from '../../../ptyp'

class ShipInfoView extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipGraphSource: PTyp.string.isRequired,
  }

  render() {
    const {mstId, shipGraphSource} = this.props
    return (
      <img
        style={{width: 218, height: 300}}
        src={shipGraphSource}
        alt={`Data not yet available for ${mstId}`}
      />
    )
  }
}

export { ShipInfoView }
