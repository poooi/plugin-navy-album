import React, { PureComponent } from 'react'
import { join } from 'path-extra'

import { PTyp } from '../../../ptyp'

class StatIcon extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    name: PTyp.string.isRequired,
  }

  render() {
    const {style, name} = this.props
    return (
      <img
        style={style}
        src={
          join(__dirname,'..','..','..','assets','icons',`${name}.png`)
        }
        alt={`stat-name`}
      />
    )
  }
}

export { StatIcon }
