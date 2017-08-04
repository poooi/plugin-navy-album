import React, { PureComponent } from 'react'
import { join } from 'path-extra'

import { PTyp } from '../../../ptyp'

class Icon extends PureComponent {
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
        alt={`icon-${name}`}
      />
    )
  }
}

export { Icon }
