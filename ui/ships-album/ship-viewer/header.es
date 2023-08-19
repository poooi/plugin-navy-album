import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { mergeMapStateToProps } from 'subtender'
import { createStructuredSelector } from 'reselect'

import { headerInfoSelector } from './selectors'
import {
  debuffFlagSelector,
} from '../selectors'

import { PTyp } from '../../../ptyp'
import { ShipGraphView } from '../../ship-graph-view'

@connect(
  mergeMapStateToProps(
    headerInfoSelector,
    createStructuredSelector({
      debuffFlag: debuffFlagSelector,
    }))
)
class Header extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipName: PTyp.string.isRequired,
    typeName: PTyp.string.isRequired,
    yomi: PTyp.string.isRequired,
    ours: PTyp.bool.isRequired,
    debuffFlag: PTyp.bool.isRequired,
    isSpecialCG: PTyp.bool.isRequired,
  }

  render() {
    const {
      mstId, shipName, typeName, yomi, ours,
      debuffFlag, isSpecialCG,
    } = this.props
    /*
       ShipGraphView requests the image automatically
       and header always show as long as the component is mounted
       so we don't need extra mechanism for requesting fetching & swf parsing
     */
    return (
      <div style={{display: 'flex', alignItems: 'center', height: 40}}>
        <div style={{display: 'flex', alignItems: 'baseline', flex: 1}}>
          <div
            style={{
              fontSize: '1.2em',
              alignSelf: 'flex-start',
            }}
          >
            {typeName}
          </div>
          <div
            style={{
              marginLeft: '.4em',
              fontSize: '1.8em',
              fontWeight: 'bold',
            }}>
            {shipName} {!ours && yomi}
          </div>
          <div
            style={{
              marginLeft: '.6em',
              fontSize: '1em',
            }}>
            ({mstId})
          </div>
        </div>
        <ShipGraphView
          key={mstId}
          mstId={mstId}
          debuffFlag={debuffFlag}
          graphType="banner"
          damaged={false}
          hideOnNoSrc
          style={
            /*
               for special master ids, ShipGraphView still works
               as automatic request sender but is invisible
             */
            isSpecialCG ?
              {display: 'none'} :
              {width: 160, height: 40}
          }
        />
      </div>
    )
  }
}

export { Header }
