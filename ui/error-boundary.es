import React, { PureComponent, Fragment } from 'react'
import { Button, EditableText } from '@blueprintjs/core'
import { clipboard } from 'electron'

import { PTyp } from '../ptyp'

const handleCopy = errorText => () =>
  clipboard.writeText(errorText)

class ErrorBoundary extends PureComponent {
  static propTypes = {
    children: PTyp.node.isRequired,
  }

  state = {
    // {error, info} if present
    errorContext: null,
  }

  static getDerivedStateFromError(error) {
    return {errorContext: {error, info: {componentStack: '<No info.componentStack available>'}}}
  }

  componentDidCatch = (error, info) =>
    this.setState({errorContext: {error, info}})

  render() {
    const {errorContext} = this.state
    if (errorContext !== null) {
      const {error, info} = errorContext
      const code = [error.stack, info.componentStack].join('\n')
      return (
        <div
          style={{
            height: '100%',
            display: 'flex',
            flexDirection: 'column',
          }}
        >
          <h1>Error encountered</h1>
          <EditableText
            intent="danger"
            multiline
            minLines={16}
            value={code}
            style={{flex: 1}}
          />
          <Button
            intent="primary"
            onClick={handleCopy(code)}
            text="Copy to clipboard"
          />
        </div>
      )
    }

    const {children} = this.props
    return (
      <Fragment>{children}</Fragment>
    )
  }
}

export { ErrorBoundary }
