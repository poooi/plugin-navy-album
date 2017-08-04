import _ from 'lodash'
import React, { PureComponent } from 'react'
import { Table } from 'react-bootstrap'

import { PTyp } from '../../../ptyp'
import { Icon } from '../../icon'
import { interpretRange, interpretSpeed } from '../../../game-misc'

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

    const decorateValue = (value, statName) => {
      if (statName === 'range')
        return interpretRange(Number(value))
      if (statName === 'speed')
        return interpretSpeed(Number(value))
      return value
    }

    const displayStats =
      'hp fire armor torpedo evasion antiair cap asw speed los range luck'
        .split(' ').map(name => ({
          statName: name,
          value: decorateValue(stats[name], name),
        }))

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
                  _.flatMap(rowStats, ({statName,value}, ind2) => [
                    <td
                      style={{
                        width: '20%',
                        ...tdStyle,
                      }}
                      key={`icon-${id(ind2)}`}>
                      <Icon
                        style={{height: '1.2em', width: 'auto'}}
                        name={statName}
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
