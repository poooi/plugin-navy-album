import { createStructuredSelector } from 'reselect'
import React, { PureComponent } from 'react'
import { modifyObject } from 'subtender'
import {
  ListGroup,
  ListGroupItem,
} from 'react-bootstrap'
import { connect } from 'react-redux'

import {
  grouppedMapIdsSelector,
} from '../../selectors'
import {
  focusedListInfoSelector,
} from './selectors'
import { PTyp } from '../../ptyp'
import { mapDispatchToProps } from '../../store'

class MapBgmViewerImpl extends PureComponent {
  static propTypes = {
    grouppedMapIds: PTyp.array.isRequired,
    listInfo: PTyp.object.isRequired,
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
    const {grouppedMapIds, listInfo} = this.props
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
            listInfo.type === 'simple' ? (
              listInfo.list.map(x => (
                <ListGroupItem
                  key={x}
                >
                  {x}
                </ListGroupItem>
              ))
            ) : (
              <div>{JSON.stringify(listInfo.list)}</div>
            )
          }
        </ListGroup>
      </div>
    )
  }
}

const MapBgmViewer = connect(
  createStructuredSelector({
    grouppedMapIds: grouppedMapIdsSelector,
    listInfo: focusedListInfoSelector,
  }),
  mapDispatchToProps,
)(MapBgmViewerImpl)

export {
  MapBgmViewer,
}
