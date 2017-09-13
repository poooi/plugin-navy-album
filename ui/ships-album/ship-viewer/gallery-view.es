import _ from 'lodash'
// import { shell } from 'electron'
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

const downloadUrl =
  window.remote.getCurrentWebContents().downloadURL

class GalleryViewImpl extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipGraphSources: PTyp.object.isRequired,
    lastFetch: PTyp.number,
    isFetching: PTyp.bool.isRequired,
    requestShipGraph: PTyp.func.isRequired,
    style: PTyp.object.isRequired,
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
      style,
    } = this.props
    const characterIds =
      Object.keys(shipGraphSources).map(Number).sort(generalComparator)
    const {__} = window
    return (
      <ListGroup style={style}>
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
                {__('ShipsTab.LastUpdate')}: {String(new Date(lastFetch))}
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
          characterIds.map(chId => {
            const src = _.get(shipGraphSources,chId,'')
            return (
              <ListGroupItem
                key={chId}
                style={{
                  textAlign: 'center',
                }}>
                <img
                  style={{maxWidth: '100%', height: 'auto'}}
                  src={src}
                  alt={`ship=${mstId}, chId=${chId}`}
                />
                {
                  src && (
                    <div style={{display: 'flex', flexDirection: 'row-reverse'}}>
                      <Button
                        onClick={() => downloadUrl(src)}
                        bsSize="small">
                        <FontAwesome name="download" />
                      </Button>
                    </div>
                  )
                }
              </ListGroupItem>
            )
          })
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
