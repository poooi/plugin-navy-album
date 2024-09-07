import _ from 'lodash'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import { Card } from '@blueprintjs/core'
import FontAwesome from 'react-fontawesome'
import { MaterialIcon } from 'views/components/etc/icon'

import { PTyp } from '../../../../ptyp'
import { StatIcon } from '../../../common/icon'
import { mapDispatchToProps } from '../../../../store'

@connect(null,mapDispatchToProps)
class InfoRow extends PureComponent {
  static propTypes = {
    mstId: PTyp.number.isRequired,
    mstIdToDesc: PTyp.func.isRequired,
    detail: PTyp.object.isRequired,
    style: PTyp.object.isRequired,
    uiSwitchShip: PTyp.func.isRequired,
  }

  render() {
    const {detail, mstId, mstIdToDesc, style, uiSwitchShip} = this.props
    const mkShipView = curMstId => {
      const {shipName, typeName} = mstIdToDesc(curMstId)
      const className = curMstId === mstId ? 'text-primary' : ''
      const hasSwitchAction = curMstId !== mstId
      return (
        <div
          style={{
            width: '30%', flex: 3,
            ...(hasSwitchAction ? {cursor: 'pointer'} : {}),
          }}
          onClick={
            hasSwitchAction ?
              () => uiSwitchShip(curMstId) :
              null
          }
        >
          <div className={className}>{typeName}</div>
          <div
            className={className}
            style={{fontSize: '1.6em', marginLeft: '.3em'}}>
            {shipName}
          </div>
        </div>
      )
    }
    return (
      <Card
        style={style}
      >
        <div
          style={{
            display: 'flex',
            alignItems: 'center',
          }}
        >
          <span style={{width: '3em', fontSize: '1.6em', flex: 1}}>
            Lv. {detail.level}
          </span>
          {mkShipView(detail.mstIdBefore)}
          <FontAwesome
            name="arrow-right"
            style={{
              margin: '.2em .8em',
            }}
          />
          {mkShipView(detail.mstIdAfter)}
        </div>
        <div style={{display: 'flex', alignItems: 'center'}}>
          {
            _.flatMap(
              'ammo steel devMat instantBuild blueprint catapult report gunMat aviationMat armsMat boiler screw techMat'
                .split(' '),
              itemOrResource => {
                const matIds = {
                  ammo: 2,
                  steel: 3,
                  instantBuild: 5,
                  devMat: 7,
                  screw: 8,
                }
                const v = detail[itemOrResource]
                if (v === 0)
                  return []
                const icon = matIds[itemOrResource] ? (
                  <MaterialIcon
                    materialId={matIds[itemOrResource]}
                    className="material-icon"
                  />
                ) : (
                  <StatIcon
                    style={{width: '1.5em', height: '1.5em'}}
                    name={itemOrResource} />
                )
                const text =
                  ['ammo', 'steel'].includes(itemOrResource) ?
                    String(v) :
                    `x${v}`

                return [
                  <span
                    key={itemOrResource}
                    style={{
                      display: 'flex',
                      alignItems: 'center',
                      marginRight: '.5em',
                    }}>
                    {icon}
                    <span>
                      {text}
                    </span>
                  </span>,
                ]
              }
            )
          }
        </div>
      </Card>
    )
  }
}

export { InfoRow }
