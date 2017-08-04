import React, { PureComponent } from 'react'
import { join } from 'path-extra'

import { PTyp } from '../ptyp'

/* eslint-disable quote-props */
const alias = {
  'anti-air': 'antiair',
  'anti-bomber': 'accuracy',
  'antibomber': 'accuracy',
  'interception': 'evasion',
}
/* eslint-enable quote-props */

class Icon extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    name: PTyp.string.isRequired,
  }

  render() {
    const {style, name} = this.props
    const fileName = alias[name] ? alias[name] : name
    return (
      <img
        style={style}
        src={
          join(__dirname,'..','assets','icons',`${fileName}.png`)
        }
        alt={`icon-${name}`}
      />
    )
  }
}

export { Icon }
