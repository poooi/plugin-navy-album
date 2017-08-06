import { createStructuredSelector } from 'reselect'
import { mergeMapStateToProps } from 'subtender'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import {
  Panel,
} from 'react-bootstrap'

import {
  gameUpdateSelector,
  serverIpSelector,
} from '../../selectors'
import { PTyp } from '../../ptyp'
import { ShipGraphView } from './ship-graph-view'

class GameUpdateViewerImpl extends PureComponent {
  static propTypes = {
    digest: PTyp.object,
    summary: PTyp.object,
    serverIp: PTyp.string.isRequired,
  }

  static defaultProps = {
    digest: null,
    summary: null,
  }

  render() {
    const {digest, summary, serverIp} = this.props
    const equipMstIdToSrc = mstId => {
      const mstIdStr = String(mstId).padStart(3,'0')
      const prefix = `http://${serverIp}/kcs/resources/image/slotitem/`
      return `${prefix}card/${mstIdStr}.png`
    }

    return (
      <div style={{height: '100%', display: 'flex', flexDirection: 'column'}}>
        <Panel style={{marginBottom: 8, flex: 1}}>
          {
            summary && (
              <div>
                {
                  summary.addedShipMstIds.length > 0 && (
                    <div>
                      <h3>New Ships</h3>
                      <div>
                        {
                          summary.addedShipMstIds.map(mstId => (
                            <ShipGraphView
                              key={mstId}
                              mstId={mstId}
                              characterId={1}
                            />
                          ))
                        }
                      </div>
                    </div>
                  )
                }
                {
                  summary.addedEquipMstIds.length > 0 && (
                    <div>
                      <h3>New Equipments</h3>
                      <div>
                        {
                          summary.addedEquipMstIds.map(mstId => (
                            <img
                              key={mstId}
                              alt={`eqp-${mstId}`}
                              style={{
                                width: 128,
                                height: 128,
                                margin: 4,
                              }}
                              src={equipMstIdToSrc(mstId)}
                            />
                          ))
                        }
                      </div>
                    </div>
                  )
                }
                {
                  summary.changedShipMstIds.length > 0 && (
                    <div>
                      <h3>New CGs</h3>
                      <div>
                        {
                          summary.changedShipMstIds.map(mstId => (
                            <ShipGraphView
                              key={mstId}
                              mstId={mstId}
                              characterId={1}
                            />
                          ))
                        }
                      </div>
                    </div>
                  )
                }
              </div>
            )
          }
          {
            digest && (
              <div>
                <h3>General Info</h3>
                <p>
                  {
                    [
                      `Ship Count:`,
                      `${digest.shipDigests.length},`,
                      `Equipment Count:`,
                      `${digest.equipMstIds.length}`,
                    ].join(' ')
                  }
                </p>
              </div>
            )
          }
        </Panel>
      </div>
    )
  }
}

const GameUpdateViewer = connect(
  mergeMapStateToProps(
    gameUpdateSelector,
    createStructuredSelector({
      serverIp: serverIpSelector,
    })
  )
)(GameUpdateViewerImpl)

export { GameUpdateViewer }
