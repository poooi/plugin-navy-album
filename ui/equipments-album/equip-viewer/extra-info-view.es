import _ from 'lodash'
import React, { PureComponent } from 'react'
import { HTMLTable } from '@blueprintjs/core'

import { PTyp } from '../../../ptyp'
import { MatIcon } from '../../common/icon'

class ExtraInfoView extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    $equip: PTyp.object.isRequired,
  }

  render() {
    const {$equip, style} = this.props
    const tdStyle = {
      textAlign: 'center',
      verticalAlign: 'middle',
    }

    const shouldShowAircraftStats =
      _.isInteger($equip.api_cost) ||
      _.isInteger($equip.api_distance)
    const {__} = window.i18n["poi-plugin-navy-album"]
    return (
      <HTMLTable
        className="extra-info-view"
        style={{
          tableLayout: 'fixed',
          width: '100%',
          ...style,
        }}
        striped bordered condensed hover>
        <tbody>
          <tr>
            <td
              style={{
                ...tdStyle,
                width: `40%`,
              }}>
              {__('EquipmentsTab.ScrapValue')}
            </td>
            <td>
              <div style={{
                display: 'flex',
                justifyContent: 'space-around',
                alignItems: 'center',
              }}>
                {
                  _.compact($equip.api_broken).length > 0 ? (
                    _.zip(
                      $equip.api_broken,
                      [1,2,3,4]
                    ).map(([v, matId]) => v > 0 && (
                      <span
                        key={matId}
                        style={{
                          ...tdStyle,
                        }}>
                        <div style={{
                          display: 'flex', alignItems: 'center',
                        }}>
                          <MatIcon materialId={matId} />
                          <span>{v}</span>
                        </div>
                      </span>
                    ))
                  ) : (
                    // no scrap value to show
                    <span>-</span>
                  )
                }
              </div>
            </td>
          </tr>
          {
            shouldShowAircraftStats && [
              <tr key="dist">
                <td style={tdStyle}>
                  {__('EquipmentsTab.Distance')}
                </td>
                <td style={tdStyle}>
                  {$equip.api_distance}
                </td>
              </tr>,
              <tr key="cost">
                <td style={tdStyle}>
                  {__('EquipmentsTab.Cost')}
                </td>
                <td style={tdStyle}>
                  {$equip.api_cost}
                </td>
              </tr>,
            ]
          }
        </tbody>
      </HTMLTable>
    )
  }
}

export { ExtraInfoView }
