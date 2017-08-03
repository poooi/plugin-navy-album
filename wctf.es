import _ from 'lodash'
import { readFileSync } from 'fs'
import { join } from 'path-extra'

const ships = _.keyBy(
  _.flatMap(
    readFileSync(
      join(__dirname, 'assets', 'wctf', 'ships.nedb'),
      {encoding: 'utf8'}
    ).split('\n'),
    raw => {
      const trimmed = raw.trim()
      return trimmed.length > 0 ? [JSON.parse(trimmed)] : []
    }),
  'id'
)

export { ships }
