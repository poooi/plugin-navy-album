import _ from 'lodash'
import React, { PureComponent } from 'react'
import { createStructuredSelector } from 'reselect'
import { connect } from 'react-redux'
import {
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
  isFetchingGraphSelector,
} from './selectors'
import { mapDispatchToProps } from '../../../store'

class GalleryViewImpl extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipGraphSources: PTyp.object.isRequired,
    lastFetch: PTyp.number,
    isFetching: PTyp.bool.isRequired,
    requestShipGraph: PTyp.func.isRequired,
  }

  static defaultProps = {
    lastFetch: null,
  }

  handleRequestUpdate = () => {
    const {mstId, requestShipGraph} = this.props
    requestShipGraph(mstId, true)
  }

  render() {
    const {
      mstId, shipGraphSources,
      lastFetch, isFetching,
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
              <Button
                disabled={isFetching}
                onClick={this.handleRequestUpdate}
                bsSize="small" bsStyle="warning">
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
    isFetching: isFetchingGraphSelector,
  }),
  mapDispatchToProps,
)(GalleryViewImpl)


export { GalleryView }
