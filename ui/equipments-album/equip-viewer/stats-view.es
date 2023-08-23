import _ from 'lodash'
import React, { PureComponent } from 'react'
import { HTMLTable } from '@blueprintjs/core'

import { PTyp } from '../../../ptyp'
import { StatIcon } from '../../common/icon'
import { interpretRange } from '../../../game-misc'

/*
   [
     [[armor, souk], [fire, houg]],
     ...
   ]
 */
const statsShape = [
  'armor souk fire houg',
  'torpedo raig bombing baku',
  'antiair tyku asw tais',
  'accuracy houm evasion houk',
  'los saku range leng',
].map(xs => _.chunk(xs.split(' '),2))

const id = _.identity

class StatsView extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    $equip: PTyp.object.isRequired,
  }

  render() {
    const {style, $equip} = this.props
    const tdStyle = {
      textAlign: 'center',
      verticalAlign: 'middle',
    }

    const decorateValue = (value, statName) => {
      if (! _.isInteger(value) || value === 0)
        return ''
      if (statName === 'range') {
        return interpretRange(value)
      }

      if (value < 0) {
        return <span className="text-danger">{value}</span>
      }

      return String(value)
    }

    return (
      <HTMLTable
        style={{
          tableLayout: 'fixed',
          width: '100%',
          ...style,
        }}
        striped bordered condensed hover>
        <tbody>
          {
            statsShape.map((statLine,lineInd) => (
              <tr key={id(lineInd)}>
                {
                  _.flatMap(
                    statLine,
                    ([statName, rawStatName], ind2) => [
                      <td
                        style={{
                          width: '20%',
                          ...tdStyle,
                        }}
                        key={`icon-${id(ind2)}`}>
                        <StatIcon
                          style={{height: '1.2em', width: 'auto'}}
                          name={statName}
                        />
                      </td>,
                      <td
                        style={{
                          width: '30%',
                          ...tdStyle,
                          fontWeight: 'bold',
                        }}
                        key={`value-${id(ind2)}`}>
                        {
                          decorateValue(
                            $equip[`api_${rawStatName}`],
                            statName
                          )
                        }
                      </td>,
                    ]
                  )
                }
              </tr>
            ))
          }
        </tbody>
      </HTMLTable>
    )
  }
}

export { StatsView }
