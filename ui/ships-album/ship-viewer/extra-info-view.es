import _ from 'lodash'
import React, { PureComponent } from 'react'
import { HTMLTable } from '@blueprintjs/core'

import { PTyp } from '../../../ptyp'
import { MatIcon, StatIcon } from '../../common/icon'

class ExtraInfoView extends PureComponent {
  static propTypes = {
    $ship: PTyp.object.isRequired,
    level: PTyp.number.isRequired,
    style: PTyp.object.isRequired,
  }

  render() {
    const {$ship, level, style} = this.props
    const applyAfterMarriage =
      v => (v === 0) ? 0 : Math.max(1, Math.floor(v*0.85))
    const costMod = level >= 100 ? applyAfterMarriage : (x => x)
    const {__} = window.i18n["poi-plugin-navy-album"]

    return (
      <HTMLTable
        bordered compact striped
        className="extra-info-view"
        style={{
          tableLayout: 'fixed',
          ...style,
        }}
      >
        <tbody>
          <tr>
            <td style={{verticalAlign: 'middle'}}>
              {__('ShipsTab.MaxConsump')}
            </td>
            {
              _.zip(
                [$ship.api_fuel_max, $ship.api_bull_max].map(costMod),
                [1,2]
              ).map(([v, matId]) => (
                <td key={matId} style={{width: '18%'}}>
                  <div style={{display: 'flex', alignItems: 'center'}}>
                    <MatIcon materialId={matId} />
                    <span className={level <= 99 ? '' : 'custom text-primary'}>
                      {v}
                    </span>
                  </div>
                </td>
              ))
            }
            <td style={{width: '18%', visibility: 'hidden'}} />
            <td style={{width: '18%', visibility: 'hidden'}} />
          </tr>
          <tr>
            <td style={{verticalAlign: 'middle'}}>
              {__('ShipsTab.ScrapValue')}
            </td>
            {
              _.zip(
                $ship.api_broken,
                [1,2,3,4]
              ).map(([v, matId]) => (
                <td key={matId}>
                  <div style={{display: 'flex', alignItems: 'center'}}>
                    <MatIcon materialId={matId} />
                    <span>{v}</span>
                  </div>
                </td>
              ))
            }
          </tr>
          <tr>
            <td style={{verticalAlign: 'middle'}}>
              {__('ShipsTab.Modernization')}
            </td>
            {
              _.zip(
                $ship.api_powup,
                'fire torpedo antiair armor'.split(' ')
              ).map(([v, statName]) => (
                <td key={statName}>
                  <div style={{display: 'flex', alignItems: 'center'}}>
                    <StatIcon
                      style={{
                        height: '1.4em',
                        width: '1.4em',
                        marginRight: '.2em',
                      }}
                      name={statName}
                    />
                    <span>{v}</span>
                  </div>
                </td>
              ))
            }
          </tr>
        </tbody>
      </HTMLTable>
    )
  }
}

export { ExtraInfoView }
