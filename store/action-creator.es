import _ from 'lodash'
import { bindActionCreators } from 'redux'
import { store } from 'views/create-store'
import { modifyObject } from 'subtender'
import { mkRequestShipGraph } from './request-ship-graph'

const actionCreator = {
  uiReady: newState => ({
    type: '@poi-plugin-navy-album@ui@Ready',
    newState,
  }),
  uiModify: modifier => ({
    type: '@poi-plugin-navy-album@ui@Modify',
    modifier,
  }),
  swfDatabaseModify: modifier => ({
    type: '@poi-plugin-navy-album@swfDatabase@Modify',
    modifier,
  }),
  uiSwitchShip: mstId =>
    actionCreator.uiModify(
      modifyObject(
        'shipsAlbum',
        modifyObject(
          'shipViewer',
          sv => {
            if (sv.mstId === mstId)
              return sv

            // disallow voice tab for abyssal ships
            const newActiveTab =
              (mstId > 1500 && sv.activeTab === 'voice') ?
                'info' : sv.activeTab

            return {
              ...sv,
              mstId,
              level: 99,
              debuffFlag: false,
              activeTab: newActiveTab,
            }
          }
        )
      )
    ),
  uiSwitchEquip: mstId =>
    actionCreator.uiModify(
      modifyObject(
        'equipmentsAlbum',
        modifyObject(
          'equipViewer',
          modifyObject(
            'mstId', () => mstId
          )
        )
      )
    ),

  swfDatabaseLockPath: path =>
    actionCreator.swfDatabaseModify(
      modifyObject(
        'fetchLocks', fl => [...fl, path]
      )
    ),
  swfDatabaseUnlockPath: path =>
    actionCreator.swfDatabaseModify(
      modifyObject(
        'fetchLocks', fl => fl.filter(p => p !== path)
      )
    ),
  swfDatabaseInsertImage: (path, characterId, img) =>
    actionCreator.swfDatabaseModify(
      modifyObject(
        'db',
        modifyObject(
          path,
          (record = {}) =>
            modifyObject(
              characterId,
              () => img
            )(record)
        )
      )
    ),
  /*
     sgInfo is {mstId, sgFileName, sgVersion, characterId, debuffFlag, img}
     where 'debuffFlag' is required to be a boolean if mstId suggests an abyssal ship
   */
  swfDatabaseInsertShipGraph: sgInfo => {
    const {mstId} = sgInfo
    const timestamp = Number(new Date())
    // as the creation might be unnecessary, delayed as thunk.
    const mkEmptyShipDbRecord = () => ({
      sgFileName: sgInfo.sgFileName,
      sgVersion: sgInfo.sgVersion,
      images: {},
      lastFetch: timestamp,
      ...(mstId <= 1500 ? {} : {imagesDebuffed: {}}),
    })

    return actionCreator.swfDatabaseModify(
      modifyObject(
        'shipDb',
        modifyObject(
          mstId,
          _.flow([
            // (1) fill or update
            /* eslint-disable indent */
            (shipDbRecord = mkEmptyShipDbRecord()) =>
              (
                // sgFileName and sgVersion as digest of the data
                shipDbRecord.sgFileName !== sgInfo.sgFileName ||
                shipDbRecord.sgVersion !== sgInfo.sgVersion
              ) ? (
                // mismatched, probably we are updating old data
                // wiping old data
                mkEmptyShipDbRecord()
              ) : (
                // keep intact
                shipDbRecord
              ),
            /* eslint-enable indent */
            // (2) update lastFetch
            modifyObject('lastFetch', () => timestamp),
            // (3) update image
            modifyObject(
              // determine update target: either 'images' or 'imagesDebuffed'
              (mstId > 1500 && sgInfo.debuffFlag) ?
                'imagesDebuffed' : 'images',
              _.flow([
                // norm
                (record = {}) => record,
                // update
                modifyObject(
                  sgInfo.characterId,
                  () => sgInfo.img
                ),
              ])
            ),
          ])
        )
      )
    )
  },
  subtitleModify: modifier => ({
    type: '@poi-plugin-navy-album@subtitle@Modify',
    modifier,
  }),
}

actionCreator.requestShipGraph =
  mkRequestShipGraph(actionCreator)

const mapDispatchToProps = _.memoize(dispatch =>
  bindActionCreators(actionCreator, dispatch))

const withBoundActionCreator = (func, dispatch=store.dispatch) =>
  func(mapDispatchToProps(dispatch))

const asyncBoundActionCreator = (func, dispatch=store.dispatch) =>
  dispatch(() => setTimeout(() =>
    withBoundActionCreator(func, dispatch)))

export {
  actionCreator,
  mapDispatchToProps,
  withBoundActionCreator,
  asyncBoundActionCreator,
}
