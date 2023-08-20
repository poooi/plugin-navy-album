import _ from 'lodash'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import React, { PureComponent } from 'react'
import FontAwesome from 'react-fontawesome'
import { connect } from 'react-redux'
import styled from 'styled-components'
import { Card } from '@blueprintjs/core'
import { SlotitemIcon } from 'views/components/etc/icon'
import { constSelector } from 'views/utils/selectors'
import { PTyp } from '../../../ptyp'
import { mapDispatchToProps } from '../../../store'

const EqpIcon = styled(SlotitemIcon)`
  &.svg {
    height: 1.2em;
  }

  &.png {
    height: 1.6em;
  }
`

@connect(
  createStructuredSelector({
    $equips: createSelector(
      constSelector,
      ({$equips}) => $equips),
  }),
  mapDispatchToProps
)
class EquipmentsView extends PureComponent {
  static propTypes = {
    slotCount: PTyp.number.isRequired,
    equips: PTyp.array.isRequired,
    $equips: PTyp.object.isRequired,
    style: PTyp.object.isRequired,
    uiSwitchEquip: PTyp.func.isRequired,
  }

  render() {
    const {slotCount, equips, $equips, style, uiSwitchEquip} = this.props
    if (equips.length > 5) {
      console.warn(`equips Array is longer than expected`, equips)
    }
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
          [0,1,2,3,4].map(slotInd => {
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
            let displayStar = null
            let onClick = null
            if (slotInd < slotCount) {
              const {mstId, star} = equips[slotInd]
              const cap = _.get(equips, [slotInd, 'cap'], 0)
              capText = String(cap)
              if (_.isInteger(mstId)) {
                const $equip = $equips[mstId]
                const iconId = $equip.api_type[3]
                onClick = () => uiSwitchEquip(mstId)
                displayIcon = (<EqpIcon slotitemId={iconId} />)
                displayName = $equip.api_name

                // number & greater than 0
                if (star) {
                  displayStar = (
                    <span
                      style={{
                        color: '#45a9a5',
                        marginRight: '.4em',
                      }}
                    >
                      {
                        star === 10 ? '★Mx' : `★+${star}`
                      }
                    </span>
                  )
                }
              } else {
                displayIcon = <span />
                displayName = <span />
              }
            } else {
              return (
                <Card
                  style={{
                    ...commonStyle,
                    textAlign: 'center',
                  }}
                  key={slotInd}>
                  <FontAwesome name="lock" />
                </Card>
              )
            }
            return (
              <Card
                key={slotInd}
                onClick={onClick}
                style={{
                  ...commonStyle,
                  display: 'flex',
                  alignItems: 'center',
                  ...(
                    typeof onClick === 'function' ? {cursor: 'pointer'} : {}
                  ),
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
                  textOverflow: 'ellipsis',
                  whiteSpace: 'nowrap',
                  overflow: 'hidden',
                }}>
                  {displayName}
                </span>
                {displayStar}
              </Card>
            )
          })
        }
      </div>
    )
  }
}

export { EquipmentsView }
