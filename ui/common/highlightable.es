import React, { PureComponent } from 'react'
import { Classes } from '@blueprintjs/core'
import { PTyp } from '../../ptyp'

/*
  (hacky) pretend that this is a Callout
  so we can get blueprint theme color applied.
 */
const highlightProps = {
  className: `${Classes.CALLOUT} ${Classes.INTENT_PRIMARY}`,
}

class Highlightable extends PureComponent {
  static propTypes = {
    content: PTyp.node.isRequired,
    highlight: PTyp.bool,
    style: PTyp.object,
  }

  static defaultProps = {
    highlight: false,
    style: {},
  }

  render() {
    const {content, highlight, style} = this.props
    return (
      <div
        style={style}
        {...(highlight ? highlightProps : {})}
      >
        {content}
      </div>
    )
  }
}

export { Highlightable }
