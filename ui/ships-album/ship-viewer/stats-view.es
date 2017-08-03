import _ from 'lodash'
import React, { PureComponent } from 'react'
import { Table } from 'react-bootstrap'

import { PTyp } from '../../../ptyp'

class StatsView extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    $ship: PTyp.object.isRequired,
    wctfShip: PTyp.object,
  }

  static defaultProps = {
    wctfShip: null,
  }

  render() {
    const {style, $ship, wctfShip} = this.props
    const id = x => x
    const stats = []
    const defineStat = (name, text) => stats.push({name, text})
    const ranged = propName => {
      const [x,y] = $ship[`api_${propName}`]
      return x === y ? `${x}` : `${x}~${y}`
    }

    // TODO: ev & los & asw are level-dependent
    const tmp = path => {
      const v = _.get(wctfShip, path)
      return _.isInteger(v) ? `${v} (at Lv. 99)` : '???'
    }

    defineStat('HP', $ship.api_taik[0])
    defineStat('FP', ranged('houg'))
    defineStat('AR', ranged('souk'))
    defineStat('TP', ranged('raig'))
    defineStat('EV', tmp('stat.evasion_max'))
    defineStat('AA', ranged('tyku'))
    defineStat('CAP', _.sum($ship.api_maxeq.filter(x => _.isInteger(x) && x > 0)))
    defineStat('ASW', tmp('stat.asw_max'))
    defineStat('SPD', String($ship.api_soku))
    defineStat('LOS', tmp('stat.los_max'))
    defineStat('RND', String($ship.api_leng))
    defineStat('LUK', ranged('luck'))

    return (
      <Table
        style={{
          tableLayout: 'fixed',
          ...style,
        }}
        striped bordered condensed hover>
        <thead>
          <tr style={{visibility: 'hidden'}}>
            <td style={{width: '20%'}} />
            <td style={{width: '30%'}} />
            <td style={{width: '20%'}} />
            <td style={{width: '30%'}} />
          </tr>
        </thead>
        <tbody>
          {
            _.chunk(stats,2).map((rowStats,ind) => (
              <tr key={id(ind)}>
                {
                  _.flatMap(rowStats, ({name,text}, ind2) => [
                    <td key={`name-${id(ind2)}`}>
                      {name}
                    </td>,
                    <td key={`text-${id(ind2)}`}>
                      {text}
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
