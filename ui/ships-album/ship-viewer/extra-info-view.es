import _ from 'lodash'
import React, { PureComponent } from 'react'
import { Table } from 'react-bootstrap'

import { MaterialIcon } from 'views/components/etc/icon'

import { PTyp } from '../../../ptyp'
import { Icon } from '../../icon'

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
      <Table
        className="extra-info-view"
        style={{
          tableLayout: 'fixed',
          ...style,
        }}
        striped bordered condensed hover>
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
                    <MaterialIcon
                      materialId={matId}
                      className="material-icon"
                    />
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
                    <MaterialIcon
                      materialId={matId}
                      className="material-icon"
                    />
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
                    <Icon
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
      </Table>
    )
  }
}

export { ExtraInfoView }
