import { createStructuredSelector } from 'reselect'
import React, { Component } from 'react'
import { connect } from 'react-redux'
import { mergeMapStateToProps, modifyObject, not } from 'subtender'
import {
  ListGroup,
  ListGroupItem,
  Label,
  Button,
} from 'react-bootstrap'

import {
  voiceListSelector,
  quotesOptionsSelector,
} from './selectors'
import {
  poiVolumeSelector,
} from '../../../../selectors'
import { PTyp } from '../../../../ptyp'
import { mapDispatchToProps } from '../../../../store'

class QuotesViewImpl extends Component {
  static propTypes = {
    voiceList: PTyp.array.isRequired,
    volume: PTyp.number.isRequired,
    showWedding: PTyp.bool.isRequired,
    showSunk: PTyp.bool.isRequired,
    uiModify: PTyp.func.isRequired,
  }

  handleToggleOption = which => () =>
    this.props.uiModify(
      modifyObject(
        'shipsAlbum',
        modifyObject(
          'shipViewer',
          modifyObject(
            'quotesOptions',
            modifyObject(which, not)
          )
        )
      )
    )

  handleCanPlay = e => {
    e.target.volume = this.props.volume
  }

  render() {
    const {voiceList, showWedding, showSunk} = this.props
    const optBtnStyle = {
      marginTop: 0,
      marginRight: '1em',
    }
    const {__} = window
    return (
      <ListGroup>
        <ListGroupItem
          key="control" style={{padding: '5px 10px'}}>
          <div style={{display: 'flex', alignItems: 'center'}}>
            <Button
              bsStyle={showWedding ? 'primary' : 'default'}
              onClick={this.handleToggleOption('showWedding')}
              style={optBtnStyle}>
              {__('ShipsTab.ShowWedding')}
            </Button>
            <Button
              bsStyle={showSunk ? 'primary' : 'default'}
              onClick={this.handleToggleOption('showSunk')}
              style={optBtnStyle}>
              {__('ShipsTab.ShowSunk')}
            </Button>
          </div>
        </ListGroupItem>
        {
          voiceList.map(voice => {
            const {voiceId, situation, url, mstId, line} = voice
            return (
              <ListGroupItem
                style={{
                  display: 'flex',
                  flexDirection: 'column',
                  padding: '5px 10px',
                }}
                key={`${mstId}-${voiceId}`}>
                <div>
                  <Label
                    style={{
                      minWidth: '10em',
                      fontSize: '1em',
                    }}
                    bsStyle="primary">
                    {situation}
                  </Label>
                </div>
                {
                  line && <p style={{marginTop: '.8em'}}>{line}</p>
                }
                <audio
                  className="play-control"
                  style={{width: '100%', marginTop: '.5em'}}
                  preload="none"
                  onCanPlay={this.handleCanPlay}
                  controls="controls">
                  <source src={url} type="audio/mp3" />
                </audio>
              </ListGroupItem>
            )
          })
        }
      </ListGroup>
    )
  }
}

const QuotesView = connect(
  mergeMapStateToProps(
    createStructuredSelector({
      voiceList: voiceListSelector,
      volume: poiVolumeSelector,
    }),
    quotesOptionsSelector
  ),
  mapDispatchToProps
)(QuotesViewImpl)

export { QuotesView }
