import { createStructuredSelector } from 'reselect'
import React, { PureComponent } from 'react'
import { modifyObject } from 'subtender'
import {
  Panel,
  ListGroup,
  ListGroupItem,
} from 'react-bootstrap'
import { connect } from 'react-redux'

import {
  grouppedMapIdsSelector,
  allMapBgmIdsSelector,
} from '../../selectors'
import {
  focusedBgmIdListSelector,
} from './selectors'
import { PTyp } from '../../ptyp'
import { mapDispatchToProps } from '../../store'

class MapBgmViewerImpl extends PureComponent {
  static propTypes = {
    grouppedMapIds: PTyp.array.isRequired,
    bgmIds: PTyp.array.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  handleChangeFocus = focus => () =>
    this.props.uiModify(
      modifyObject(
        'musicLibrary',
        modifyObject(
          'mapBgmViewer',
          modifyObject('focus', () => focus)
        )
      )
    )

  render() {
    const {grouppedMapIds, bgmIds} = this.props
    const itemStyle = {padding: '8px 10px'}
    return (
      <div
        style={{
          height: '100%',
          display: 'flex',
        }}
      >
        <ListGroup
          style={{
            width: '20%',
            maxWidth: '10em',
            marginBottom: 0,
            height: '100%',
            overflowY: 'auto',
          }}
        >
          <ListGroupItem
            style={itemStyle}
            onClick={this.handleChangeFocus({type: 'all'})}
          >
            All
          </ListGroupItem>
          {
            grouppedMapIds.map(grp => (
              <ListGroupItem
                onClick={this.handleChangeFocus({type: 'world', worldId: grp.area})}
                style={itemStyle}
                key={grp.area}
              >
                World #{grp.area}
              </ListGroupItem>
            ))
          }
          <ListGroupItem
            onClick={this.handleChangeFocus({type: 'others'})}
            style={itemStyle}
          >
            Others
          </ListGroupItem>
        </ListGroup>
        <ListGroup
          style={{
            flex: 1,
            height: '100%',
            marginBottom: 0,
            marginLeft: 5,
            overflowY: 'auto',
          }}
        >
          {
            bgmIds.map(x => (
              <ListGroupItem
                key={x}
              >
                {x}
              </ListGroupItem>
            ))
          }
        </ListGroup>
      </div>
    )
  }
}

const MapBgmViewer = connect(
  createStructuredSelector({
    grouppedMapIds: grouppedMapIdsSelector,
    bgmIds: focusedBgmIdListSelector,
  }),
  mapDispatchToProps,
)(MapBgmViewerImpl)

export {
  MapBgmViewer,
}
