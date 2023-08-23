import React, { PureComponent } from 'react'
import { join } from 'path-extra'
import styled from 'styled-components'

import { MaterialIcon } from 'views/components/etc/icon'

import { PTyp } from '../../ptyp'

const MatIcon = styled(MaterialIcon)`
  height: 1.4em;
  width: 1.4em;
  margin-right: .2em;
`

/* eslint-disable quote-props */
const statAlias = {
  'anti-air': 'antiair',
  'anti-bomber': 'accuracy',
  'antibomber': 'accuracy',
  'interception': 'evasion',
}
/* eslint-enable quote-props */


class StatIcon extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    name: PTyp.string.isRequired,
  }

  render() {
    const {style, name} = this.props
    const fileName = statAlias[name] ? statAlias[name] : name
    return (
      <img
        style={style}
        src={
          join(__dirname,'..','..','assets','icons',`${fileName}.png`)
        }
        alt={`icon-${name}`}
      />
    )
  }
}

export { MatIcon, StatIcon }
