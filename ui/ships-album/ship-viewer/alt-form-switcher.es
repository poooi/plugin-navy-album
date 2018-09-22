import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { constSelector } from 'views/utils/selectors'
import { Label } from 'react-bootstrap'
import { modifyObject } from 'subtender'

import {
  mstIdSelector,
  debuffFlagSelector,
  hasDebuffedGraphsSelector,
} from '../selectors'
import { remodelInfoSelector } from '../../../selectors'

import { PTyp } from '../../../ptyp'
import { mapDispatchToProps } from '../../../store'

class AltFormSwitcherImpl extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    remodelInfo: PTyp.object.isRequired,
    $ships: PTyp.object.isRequired,
    hasDebuffedGraphs: PTyp.bool.isRequired,
    debuffFlag: PTyp.bool.isRequired,
    uiSwitchShip: PTyp.func.isRequired,
    uiModify: PTyp.func.isRequired,
    touchDebuffGraph: PTyp.func.isRequired,
  }

  componentDidMount() {
    this.tryTouchDebuffGraph(this.props.mstId)
  }

  componentDidUpdate(prevProps) {
    if (prevProps.mstId !== this.props.mstId) {
      this.tryTouchDebuffGraph(this.props.mstId)
    }
  }

  tryTouchDebuffGraph(mstId) {
    const {touchDebuffGraph} = this.props
    if (mstId > 1500)
      touchDebuffGraph(mstId, false)
  }

  handleSwitchShip = mstId => () =>
    this.props.uiSwitchShip(mstId)

  handleChangeDebuffFlag = debuffFlag => () =>
    this.props.uiModify(
      modifyObject(
        'shipsAlbum',
        modifyObject(
          'shipViewer',
          modifyObject(
            'debuffFlag', () => debuffFlag
          )
        )
      )
    )

  render() {
    const {mstId} = this.props
    const {__} = window.i18n["poi-plugin-navy-album"]
    const noRender = (<div style={{display: 'none'}} />)
    if (mstId > 1500) {
      const {hasDebuffedGraphs, debuffFlag, $ships} = this.props
      if (!hasDebuffedGraphs)
        return noRender
      const abyssalName = $ships[mstId].api_name
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
            [false, true].map((curDebuffFlag,ind) => (
              <Label
                onClick={this.handleChangeDebuffFlag(curDebuffFlag)}
                bsStyle={curDebuffFlag === debuffFlag ? 'danger' : 'default'}
                key={curDebuffFlag ? 'debuffed' : 'normal'}
                style={{
                  cursor: 'pointer',
                  fontSize: '1em',
                  ...(ind === 0 ? {} : {marginLeft: '.4em'}),
                }}>
                {`${abyssalName}${curDebuffFlag ? __('ShipsTab.Debuff') : ''}`}
              </Label>
            ))
          }
        </div>
      )
    }

    const {remodelInfo, $ships} = this.props
    const {remodelChains, originMstIdOf} = remodelInfo
    const originMstId = originMstIdOf[mstId]
    if (!originMstId)
      return noRender
    const remodelChain = remodelChains[originMstId]
    if (!remodelChain)
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
    debuffFlag: debuffFlagSelector,
    hasDebuffedGraphs: hasDebuffedGraphsSelector,
    remodelInfo: remodelInfoSelector,
    $ships: createSelector(
      constSelector,
      ({$ships}) => $ships
    ),
  }),
  mapDispatchToProps,
)(AltFormSwitcherImpl)

export { AltFormSwitcher }
