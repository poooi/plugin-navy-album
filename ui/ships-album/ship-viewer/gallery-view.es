import _ from 'lodash'
import React, { Component } from 'react'
import { createStructuredSelector } from 'reselect'
import { connect } from 'react-redux'
import {
  Panel, Tab, Nav, NavItem,
  ListGroupItem, ListGroup, Button,
} from 'react-bootstrap'
import FontAwesome from 'react-fontawesome'
import { generalComparator } from 'subtender'

import { PTyp } from '../../../ptyp'
import {
  shipGraphSourcesSelector,
  mstIdSelector,
} from '../selectors'
import {
  shipGraphLastFetchSelector,
} from './selectors'
import { mapDispatchToProps } from '../../../store'

class GalleryViewImpl extends Component {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipGraphSources: PTyp.object.isRequired,
    lastFetch: PTyp.number,
    uiModify: PTyp.func.isRequired,
  }

  static defaultProps = {
    lastFetch: null,
  }

  render() {
    const {
      mstId, shipGraphSources, lastFetch,
    } = this.props
    const characterIds =
      Object.keys(shipGraphSources).map(Number).sort(generalComparator)

    return (
      <ListGroup>
        {
          lastFetch && (
            <ListGroupItem
              style={{
                paddingTop: '.4em',
                paddingBottom: '.4em',
                display: 'flex',
                alignItems: 'center',
              }}
              key="control">
              <span style={{flex: 1}}>
                Last Update: {String(new Date(lastFetch))}
              </span>
              <Button bsSize="small" bsStyle="warning">
                <FontAwesome name="refresh" />
              </Button>
            </ListGroupItem>
          )
        }
        {
          characterIds.map(chId => (
            <ListGroupItem
              key={chId}
              style={{
                textAlign: 'center',
              }}>
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
    lastFetch: shipGraphLastFetchSelector,
  }),
  mapDispatchToProps,
)(GalleryViewImpl)


export { GalleryView }
