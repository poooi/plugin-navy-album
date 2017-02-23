import React, { PureComponent, Fragment } from 'react'
import { FormControl, Button } from 'react-bootstrap'
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
          <FormControl
            componentClass="textarea"
            readOnly
            value={code}
            style={{flex: 1, height: 0}}
          />
          <Button
            bsStyle="primary"
            onClick={handleCopy(code)}
          >
            Copy to clipboard
          </Button>
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
