import _ from 'lodash'
import React, { PureComponent } from 'react'
import { Table } from 'react-bootstrap'
import { MaterialIcon } from 'views/components/etc/icon'

import { PTyp } from '../../../ptyp'
import { Icon } from '../../icon'

const id = _.identity

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
            <td
              style={{
                ...tdStyle,
                width: `40%`,
              }}>
              Scrap Value
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
                          <MaterialIcon
                            materialId={matId}
                            className="material-icon"
                          />
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
                  Distance
                </td>
                <td style={tdStyle}>
                  {$equip.api_distance}
                </td>
              </tr>,
              <tr key="cost">
                <td style={tdStyle}>
                  Cost
                </td>
                <td style={tdStyle}>
                  {$equip.api_cost}
                </td>
              </tr>,
            ]
          }
        </tbody>
      </Table>
    )
  }
}

export { ExtraInfoView }
