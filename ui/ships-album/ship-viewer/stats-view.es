import _ from 'lodash'
import React, { PureComponent } from 'react'
import { Table } from 'react-bootstrap'

import { PTyp } from '../../../ptyp'
import { Icon } from '../../icon'

class StatsView extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    stats: PTyp.object.isRequired,
  }

  static defaultProps = {
    wctfShip: null,
  }

  render() {
    const {style, stats} = this.props
    const id = x => x
    const displayStats =
      'hp fire armor torpedo evasion antiair cap asw speed los range luck'
        .split(' ').map(name =>
          ({iconName: name, value: stats[name]}))

    const tdStyle = {
      textAlign: 'center',
      verticalAlign: 'middle',
    }
    return (
      <Table
        style={{
          tableLayout: 'fixed',
          ...style,
        }}
        striped bordered condensed hover>
        <tbody>
          {
            _.chunk(displayStats,2).map((rowStats,ind) => (
              <tr key={id(ind)}>
                {
                  _.flatMap(rowStats, ({iconName,value}, ind2) => [
                    <td
                      style={{
                        width: '20%',
                        ...tdStyle,
                      }}
                      key={`icon-${id(ind2)}`}>
                      <Icon
                        style={{height: '1.2em', width: 'auto'}}
                        name={iconName}
                      />
                    </td>,
                    <td
                      style={{
                        width: '30%',
                        ...tdStyle,
                      }}
                      key={`value-${id(ind2)}`}>
                      {value}
                    </td>,
                  ])
                }
              </tr>
            ))
          }
        </tbody>
      </Table>
    )
  }
}

export { StatsView }
