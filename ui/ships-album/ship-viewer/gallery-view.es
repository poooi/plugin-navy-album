import _ from 'lodash'
import React, { Component } from 'react'
import { createStructuredSelector } from 'reselect'
import { connect } from 'react-redux'
import {
  Panel, Tab, Nav, NavItem,
  ListGroupItem, ListGroup,
} from 'react-bootstrap'
import { generalComparator } from 'subtender'

import { PTyp } from '../../../ptyp'
import {
  shipGraphSourcesSelector,
  mstIdSelector,
} from '../selectors'
import { mapDispatchToProps } from '../../../store'

class GalleryViewImpl extends Component {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipGraphSources: PTyp.object.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  render() {
    const {
      mstId, shipGraphSources,
    } = this.props
    const characterIds =
      Object.keys(shipGraphSources).map(Number).sort(generalComparator)

    return (
      <ListGroup>
        {
          characterIds.map(chId => (
            <ListGroupItem key={chId} style={{textAlign: 'center'}}>
              <img
                style={{maxWidth: '100%', height: 'auto'}}
                src={_.get(shipGraphSources,chId,'')}
                alt={`ship=${mstId}, chId=${chId}`}
              />
            </ListGroupItem>
          ))
        }
      </ListGroup>
    )
  }
}

const GalleryView = connect(
  createStructuredSelector({
    shipGraphSources: shipGraphSourcesSelector,
    mstId: mstIdSelector,
  }),
  mapDispatchToProps,
)(GalleryViewImpl)


export { GalleryView }
