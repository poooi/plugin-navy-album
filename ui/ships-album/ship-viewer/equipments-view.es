import _ from 'lodash'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import React, { PureComponent } from 'react'
import FontAwesome from 'react-fontawesome'
import { connect } from 'react-redux'
import { Label } from 'react-bootstrap'
import { SlotitemIcon } from 'views/components/etc/icon'
import { constSelector } from 'views/utils/selectors'
import { PTyp } from '../../../ptyp'

class EquipmentsViewImpl extends PureComponent {
  static propTypes = {
    slotCount: PTyp.number.isRequired,
    equips: PTyp.array.isRequired,
    $equips: PTyp.object.isRequired,
    style: PTyp.object.isRequired,
  }

  render() {
    const {slotCount, equips, $equips, style} = this.props
    return (
      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          ...style,
        }}
        className="equipments-view"
      >
        {
          [0,1,2,3].map(slotInd => {
            const commonStyle= {
              fontSize: '1.2em',
              width: '100%',
              height: '1.6em',
              lineHeight: '1.6em',
              margin: '.1em',
              padding: 0,
            }

            let capText
            let displayIcon
            let displayName
            if (slotInd < slotCount) {
              const {cap, mstId} = equips[slotInd]
              capText = String(cap)
              if (_.isInteger(mstId)) {
                const $equip = $equips[mstId]
                const iconId = $equip.api_type[3]
                displayIcon = (
                  <SlotitemIcon
                    className="slotitem-img"
                    slotitemId={iconId}
                  />
                )
                displayName = $equip.api_name
              } else {
                displayIcon = <span />
                displayName = <span />
              }
            } else {
              return (
                <Label
                  style={{
                    ...commonStyle,
                    textAlign: 'center',
                  }}
                  key={slotInd}>
                  <FontAwesome name="lock" />
                </Label>
              )
            }
            return (
              <Label
                key={slotInd}
                style={{
                  ...commonStyle,
                  display: 'flex',
                  alignItems: 'center',
                }}>
                <span style={{
                  width: '2em',
                  textAlign: 'right',
                  marginRight: '.4em',
                }}>
                  {capText}
                </span>
                <span style={{
                  width: '1.5em',
                  marginRight: '.4em',
                  textAlign: 'center',
                }}>
                  {displayIcon}
                </span>
                <span style={{
                  width: 'auto',
                  flex: 1,
                  textAlign: 'start',
                }}>
                  {displayName}
                </span>
              </Label>
            )
          })
        }
      </div>
    )
  }
}

const EquipmentsView = connect(
  createStructuredSelector({
    $equips: createSelector(
      constSelector,
      ({$equips}) => $equips),
  })
)(EquipmentsViewImpl)

export { EquipmentsView }
