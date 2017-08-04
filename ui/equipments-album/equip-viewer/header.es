import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { SlotitemIcon } from 'views/components/etc/icon'
import { enumFromTo } from 'subtender'

import {
  equipRawInfoSelector,
} from '../selectors'

import { PTyp } from '../../../ptyp'
import { Icon } from '../../icon'

class HeaderImpl extends PureComponent {
  static propTypes = {
    $equip: PTyp.object.isRequired,
    $equipType: PTyp.object.isRequired,
  }

  render() {
    const {$equip, $equipType} = this.props
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
          <SlotitemIcon
            className="slotitem-img"
            slotitemId={$equip.api_type[3]}
          />
          <div style={{display: 'flex', alignItems: 'center'}}>
            {
              enumFromTo(1,$equip.api_rare).map(x => (
                <Icon
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

const Header = connect(equipRawInfoSelector)(HeaderImpl)

export { Header }
