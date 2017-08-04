import { readJsonSync } from 'fs-extra'
import { join } from 'path-extra'
import { createSelector } from 'reselect'

import {
  constSelector,
} from 'views/utils/selectors'

const defaultShipUpgrades = readJsonSync(
  join(__dirname, '..', 'assets', 'ship-upgrades.json')
)

const shipUpgradesSelector = createSelector(
  constSelector,
  ({$shipUpgrades}) =>
    $shipUpgrades || defaultShipUpgrades
)

export { shipUpgradesSelector }
