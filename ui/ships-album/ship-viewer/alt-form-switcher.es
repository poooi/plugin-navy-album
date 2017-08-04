import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { constSelector } from 'views/utils/selectors'
import { Label } from 'react-bootstrap'

import { mstIdSelector } from '../selectors'
import { remodelInfoSelector } from '../../../selectors'

import { PTyp } from '../../../ptyp'
import { mapDispatchToProps } from '../../../store'

class AltFormSwitcherImpl extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    remodelInfo: PTyp.object.isRequired,
    $ships: PTyp.object.isRequired,
    uiSwitchShip: PTyp.func.isRequired,
  }

  handleSwitchShip = mstId => () =>
    this.props.uiSwitchShip(mstId)

  render() {
    const {mstId} = this.props
    const noRender = <div style={{display: 'none'}} />
    if (mstId > 1500) {
      // TODO: for abyssal ships show switch between
      // normal vs. debuffed, if it's possible.
      return noRender
    }

    const {remodelInfo, $ships} = this.props
    const {remodelChains, originMstIdOf} = remodelInfo
    const originMstId = originMstIdOf[mstId]
    if (! originMstId)
      return noRender
    const remodelChain = remodelChains[originMstId]
    if (! remodelChain)
      return noRender

    return (
      <div style={{
        width: '100%',
        marginBottom: '.2em',
        marginTop: '.8em',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'flex-end',
      }}>
        {
          remodelChain.map((curMstId,ind) => (
            <Label
              onClick={this.handleSwitchShip(curMstId)}
              bsStyle={curMstId === mstId ? 'primary' : 'default'}
              key={curMstId}
              style={{
                cursor: 'pointer',
                fontSize: '1em',
                ...(ind === 0 ? {} : {marginLeft: '.4em'}),
              }}>
              {$ships[curMstId].api_name}
            </Label>
          ))
        }
      </div>
    )
  }
}

const AltFormSwitcher = connect(
  createStructuredSelector({
    mstId: mstIdSelector,
    remodelInfo: remodelInfoSelector,
    $ships: createSelector(
      constSelector,
      ({$ships}) => $ships
    ),
  }),
  mapDispatchToProps,
)(AltFormSwitcherImpl)

export { AltFormSwitcher }
