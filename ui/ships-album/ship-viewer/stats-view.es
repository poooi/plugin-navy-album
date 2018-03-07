import _ from 'lodash'
import React, { PureComponent } from 'react'
import {
  Table,
  OverlayTrigger, Tooltip,
} from 'react-bootstrap'

import { PTyp } from '../../../ptyp'
import { Icon } from '../../icon'
import { interpretRange, interpretSpeed } from '../../../game-misc'

class StatsView extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    /* stats[statName] is
       - either a value to be displayed
       - or an Object of {value, tooltip}, of which
         it allows an optional "tooltip" to be used as, well, tooltip.
     */
    stats: PTyp.object.isRequired,
    // prefix for making tooltips, pass an empty one if it's not needed.
    prefix: PTyp.string.isRequired,
  }

  render() {
    const {style, stats} = this.props
    const id = x => x

    const decorateStatValue = (objOrValue, statName) => {
      const decorateValue = value => {
        if (statName === 'range')
          return interpretRange(Number(value))
        if (statName === 'speed')
          return interpretSpeed(Number(value))
        return value
      }

      if (
        objOrValue && typeof objOrValue === 'object' &&
        ('value' in objOrValue) && ('tooltip' in objOrValue)
      ) {
        const {prefix} = this.props
        const {value, tooltip} = objOrValue
        return (
          <OverlayTrigger
            placement="right"
            overlay={(
              <Tooltip id={`${prefix}${statName}`}>{tooltip}</Tooltip>
            )}
          >
            <div>{decorateValue(value)}</div>
          </OverlayTrigger>
        )
      } else {
        return decorateValue(objOrValue)
      }
    }

    const displayStats =
      'hp fire armor torpedo evasion antiair cap asw speed los range luck'
        .split(' ').map(name => ({
          statName: name,
          value: decorateStatValue(stats[name], name),
        }))

    const tdStyle = {
      textAlign: 'center',
      verticalAlign: 'middle',
    }
    return (
      <Table
        style={{
          tableLayout: 'fixed',
          marginBottom: 0,
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
