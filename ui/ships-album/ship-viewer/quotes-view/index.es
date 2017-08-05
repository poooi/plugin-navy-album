import { createStructuredSelector } from 'reselect'
import React, { Component } from 'react'
import { connect } from 'react-redux'
import {
  ListGroup,
  ListGroupItem,
} from 'react-bootstrap'

import { voiceListSelector } from './selectors'
import { PTyp } from '../../../../ptyp'

class QuotesViewImpl extends Component {
  static propTypes = {
    voiceList: PTyp.array.isRequired,
  }

  render() {
    console.log(this.props.voiceList)
    return (
      <div>
        TODO: voice player & subtitles
      </div>
    )
  }
}

const QuotesView = connect(
  createStructuredSelector({
    voiceList: voiceListSelector,
  })
)(QuotesViewImpl)

export { QuotesView }
