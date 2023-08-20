import { createStructuredSelector } from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'

import {
  mergeMapStateToProps,
} from 'subtender'

import {
  mstIdSelector,
  equipRawInfoSelector,
} from '../selectors'

import { PTyp } from '../../../ptyp'
import { ErrorBoundary } from '../../error-boundary'
import { isAbyssalEquipMstId } from '../../../game-misc'
import { Header } from './header'
import { IntroView } from './intro-view'
import { StatsView } from './stats-view'
import {
  ExtraInfoView,
} from './extra-info-view'

@connect(
  mergeMapStateToProps(
    createStructuredSelector({
      mstId: mstIdSelector,
    }),
    equipRawInfoSelector,
  )
)
class EquipViewer extends PureComponent {
  static propTypes = {
    style: PTyp.object.isRequired,
    mstId: PTyp.number.isRequired,
    $equip: PTyp.object.isRequired,
    $equipType: PTyp.object.isRequired,
  }

  render() {
    const {
      style, mstId,
      $equip, $equipType,
    } = this.props
    const infoProps = {mstId, $equip, $equipType}
    const showInfo = !isAbyssalEquipMstId(mstId)
    return (
      <div
        className="equip-viewer"
        style={{
          ...style,
        }}
      >
        <ErrorBoundary>
          <div
            style={{flex: 1, height: '100%', overflowY: 'auto'}}
          >
            <Header {...infoProps} />
            {
              showInfo && (
                <IntroView style={{}} />
              )
            }
            <div style={{
              display: 'flex',
              alignItems: 'center',
            }}>
              <div style={{
                flex: 1,
                display: 'flex',
                justifyContent: 'space-around',
                margin: 20,
              }}>
                <StatsView
                  style={{maxWidth: 380}}
                  {...infoProps}
                />
              </div>
              {
                showInfo && (
                  <div style={{
                    flex: 1,
                    display: 'flex',
                    justifyContent: 'space-around',
                    marginLeft: '1em',
                  }}>
                    <ExtraInfoView
                      style={{}}
                      {...infoProps}
                    />
                  </div>
                )
              }
            </div>
          </div>
        </ErrorBoundary>
      </div>
    )
  }
}

export { EquipViewer }
