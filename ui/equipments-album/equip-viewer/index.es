import { createStructuredSelector } from 'reselect'
import React, { PureComponent } from 'react'
import { connect } from 'react-redux'
import {
  Panel,
} from 'react-bootstrap'

import {
  mergeMapStateToProps,
} from 'subtender'

import {
  mstIdSelector,
  equipRawInfoSelector,
} from '../selectors'

import { PTyp } from '../../../ptyp'
import { ErrorBoundary } from '../../error-boundary'
import { Header } from './header'
import { IntroView } from './intro-view'
import { StatsView } from './stats-view'
import {
  ExtraInfoView,
} from './extra-info-view'

class EquipViewerImpl extends PureComponent {
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
    return (
      <Panel
        className="equip-viewer"
        style={{
          ...style,
        }}
      >
        <Panel.Body>
          <ErrorBoundary>
            <div
              style={{flex: 1, height: 0, overflowY: 'auto'}}
            >
              <Header {...infoProps} />
              {
                mstId < 501 && (
                  <IntroView style={{}} />
                )
              }
              <div style={{
                display: 'flex',
              }}>
                <div style={{
                  flex: 1,
                  display: 'flex',
                  justifyContent: 'space-around',
                }}>
                  <StatsView
                    style={{maxWidth: 380}}
                    {...infoProps}
                  />
                </div>
                {
                  mstId < 501 && (
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
        </Panel.Body>
      </Panel>
    )
  }
}

const EquipViewer = connect(
  mergeMapStateToProps(
    createStructuredSelector({
      mstId: mstIdSelector,
    }),
    equipRawInfoSelector,
  )
)(EquipViewerImpl)

export { EquipViewer }
