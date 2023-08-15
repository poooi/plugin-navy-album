import { createStructuredSelector } from 'reselect'
import React, { Component } from 'react'
import { connect } from 'react-redux'
import { mergeMapStateToProps, modifyObject, not } from 'subtender'
import {
  Button,
  Card,
  Tag,
  Classes,
} from '@blueprintjs/core'

import {
  voiceListSelector,
  quotesOptionsSelector,
} from './selectors'
import {
  poiVolumeSelector,
} from '../../../../selectors'
import { PTyp } from '../../../../ptyp'
import { mapDispatchToProps } from '../../../../store'

@connect(
  mergeMapStateToProps(
    createStructuredSelector({
      voiceList: voiceListSelector,
      volume: poiVolumeSelector,
    }),
    quotesOptionsSelector
  ),
  mapDispatchToProps
)
class QuotesView extends Component {
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
    const {__} = window.i18n["poi-plugin-navy-album"]
    return (
      <ul className={Classes.LIST}>
        <div
          key="control"
          style={{
            padding: '5px 10px',
            display: 'flex',
            alignItems: 'center',
          }}
        >
          <Button
            intent={showWedding ? 'primary' : 'default'}
            onClick={this.handleToggleOption('showWedding')}
            style={optBtnStyle}
            text={__('ShipsTab.ShowWedding')}
          />
          <Button
            intent={showSunk ? 'primary' : 'default'}
            onClick={this.handleToggleOption('showSunk')}
            style={optBtnStyle}
            text={__('ShipsTab.ShowSunk')}
          />
        </div>
        {
          voiceList.map(voice => {
            const {voiceId, situation, url, mstId, line} = voice
            return (
              <Card
                style={{
                  display: 'flex',
                  flexDirection: 'column',
                  padding: '5px 10px',
                }}
                key={`${mstId}-${voiceId}`}>
                <div>
                  <Tag
                    style={{
                      fontSize: '1em',
                    }}
                    intent="primary"
                    round
                  >
                    {situation}
                  </Tag>
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
              </Card>
            )
          })
        }
      </ul>
    )
  }
}

export { QuotesView }
