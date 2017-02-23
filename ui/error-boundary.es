import React, { PureComponent, Fragment } from 'react'
import { FormControl, Button } from 'react-bootstrap'

import { PTyp } from '../ptyp'

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
        <Fragment>
          <h1>Error encountered</h1>
          <FormControl
            componentClass="textarea"
            readOnly
            value={code}
            style={{ height: '10em' }}
          />
          <Button bsStyle="primary" onClick={this.handleCopy}>
            Copy to clipboard
          </Button>
        </Fragment>
      )
    }

    const {children} = this.props
    return (
      <Fragment>{children}</Fragment>
    )
  }
}

export { ErrorBoundary }
