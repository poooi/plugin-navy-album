import _ from 'lodash'
import { readJsonSync } from 'fs-extra'
import { join } from 'path-extra'

import React, { PureComponent } from 'react'

import { PTyp } from '../../../ptyp'
import { EquipmentsView } from './equipments-view'

// TODO: definitely needs cleanup
const abyssalInfo = readJsonSync(
  join(__dirname,'..', '..','..','assets','abyssal.json')
)

class AbyssalInfoView extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    shipGraphSource: PTyp.string.isRequired,
  }
  render() {
    const {mstId, shipGraphSource} = this.props
    const $abyssal = abyssalInfo[mstId]
    const hasAbyssalEquipsInfo =
      $abyssal &&
      Array.isArray($abyssal.EQUIPS) &&
      Array.isArray($abyssal.SLOTS)
    return (
      <div style={{margin: '.2em'}}>
        <img
          style={{maxWidth: '100%', height: 'auto'}}
          src={shipGraphSource}
          alt={`Data not yet available for ${mstId}`}
        />
        {
          hasAbyssalEquipsInfo && (
            <EquipmentsView
              style={{
                marginTop: '.4em',
                maxWidth: 270,
              }}
              slotCount={$abyssal.EQUIPS.length}
              equips={
                _.zip($abyssal.SLOTS, $abyssal.EQUIPS).map(([cap, eMstId]) =>
                  ({cap, mstId: eMstId}))
              }
            />
          )
        }
      </div>
    )
  }
}

export { AbyssalInfoView }
