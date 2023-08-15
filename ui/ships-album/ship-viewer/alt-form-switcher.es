import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { constSelector } from 'views/utils/selectors'
import { Tag } from '@blueprintjs/core'
import { modifyObject } from 'subtender'

import {
  mstIdSelector,
  debuffFlagSelector,
  hasDebuffedGraphsSelector,
} from '../selectors'
import { remodelInfoSelector } from '../../../selectors'
import { isAbyssalShipMstId } from '../../../game-misc'
import { PTyp } from '../../../ptyp'
import { mapDispatchToProps } from '../../../store'

@connect(
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
)
class AltFormSwitcher extends PureComponent {
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
    if (isAbyssalShipMstId(mstId))
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
    if (isAbyssalShipMstId(mstId)) {
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
              <Tag
                onClick={this.handleChangeDebuffFlag(curDebuffFlag)}
                intent={curDebuffFlag === debuffFlag ? 'danger' : 'default'}
                key={curDebuffFlag ? 'debuffed' : 'normal'}
                style={{
                  cursor: 'pointer',
                  fontSize: '1em',
                  ...(ind === 0 ? {} : {marginLeft: '.4em'}),
                }}>
                {`${abyssalName}${curDebuffFlag ? __('ShipsTab.Debuff') : ''}`}
              </Tag>
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
            <Tag
              onClick={this.handleSwitchShip(curMstId)}
              intent={curMstId === mstId ? 'primary' : 'default'}
              key={curMstId}
              style={{
                cursor: 'pointer',
                fontSize: '1em',
                ...(ind === 0 ? {} : {marginLeft: '.4em'}),
              }}>
              {$ships[curMstId].api_name}
            </Tag>
          ))
        }
      </div>
    )
  }
}

export { AltFormSwitcher }
