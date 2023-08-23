import React, { PureComponent } from 'react'
import { SlotitemIcon } from 'views/components/etc/icon'
import { enumFromTo } from 'subtender'
import styled from 'styled-components'

import { PTyp } from '../../../ptyp'
import { StatIcon } from '../../common/icon'

const EqpIcon = styled(SlotitemIcon)`
  &.svg {
    height: 2em;
    width: 2em;
    margin-bottom: .4em;
  }

  &.png {
    height: 2.5em;
    width: 2.5em;
  }
`

class Header extends PureComponent {
  static propTypes = {
    $equip: PTyp.object.isRequired,
    $equipType: PTyp.object.isRequired,
  }

  render() {
    const {$equip, $equipType} = this.props
    if ($equip === null || $equipType === null) {
      return (<div>???</div>)
    }

    const mstId = $equip.api_id
    return (
      <div
        className="header"
        style={{
          display: 'flex',
          alignItems: 'center',
          marginBottom: '.4em',
        }}>
        <div style={{
          flex: 1,
          display: 'flex',
          alignItems: 'baseline',
          fontSize: '1.5em',
        }}>
          <span style={{width: '4em'}}>
            {
              $equip.api_sortno > 0 ?
                `No. ${$equip.api_sortno}` : ''
            }
          </span>
          <div style={{fontSize: '.6em', alignSelf: 'flex-start'}}>
            {$equipType.api_name}
          </div>
          <div style={{fontSize: '1.1em', marginLeft: '.3em'}}>
            {$equip.api_name}
          </div>
          <div style={{fontSize: '.6em', marginLeft: '.3em'}}>
            ({mstId})
          </div>
        </div>
        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'flex-end',
          }}>
          <EqpIcon slotitemId={$equip.api_type[3]} />
          <div style={{display: 'flex', alignItems: 'center', height: '1em'}}>
            {
              enumFromTo(1,$equip.api_rare).map(x => (
                <StatIcon
                  key={x}
                  style={{height: '1em', width: 'auto'}}
                  name="star"
                />
              ))
            }
          </div>
        </div>
      </div>
    )
  }
}

export { Header }
