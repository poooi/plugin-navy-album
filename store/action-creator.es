import _ from 'lodash'
import { bindActionCreators } from 'redux'
import { store } from 'views/create-store'
import { modifyObject, generalComparator } from 'subtender'
import { mkRequestShipGraph } from './request-ship-graph'
import { actionCreators as swfCacheAC } from './ext-root/swf-cache'
import { actionCreators as masterAC } from './ext-root/master'

const actionCreator = {
  uiReady: newState => ({
    type: '@poi-plugin-navy-album@ui@Ready',
    newState,
  }),
  uiModify: modifier => ({
    type: '@poi-plugin-navy-album@ui@Modify',
    modifier,
  }),
  uiSwitchShip: mstId =>
    actionCreator.uiModify(
      _.flow([
        modifyObject(
          'activeTab', () => 'ships'
        ),
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
        ),
      ])
    ),
  uiSwitchEquip: mstId =>
    actionCreator.uiModify(
      _.flow([
        modifyObject(
          'activeTab', () => 'equipments'
        ),
        modifyObject(
          'equipmentsAlbum',
          modifyObject(
            'equipViewer',
            modifyObject(
              'mstId', () => mstId
            )
          )
        ),
      ])
    ),
  swfCacheLockPath: path =>
    actionCreator.swfCacheModify(
      modifyObject(
        'fetchLocks', fl => [...fl, path]
      )
    ),
  swfCacheUnlockPath: path =>
    actionCreator.swfCacheModify(
      modifyObject(
        'fetchLocks', fl => fl.filter(p => p !== path)
      )
    ),
  /*
     sgInfo is {mstIdX, sgFileName, sgVersion, characterId, fileName}
     where 'debuffFlag' is required to be a boolean if mstId suggests an abyssal ship
   */
  swfCacheRegisterShipGraph: sgInfo => {
    const {
      mstIdX, sgFileName, sgVersion, characterId, fileName,
    } = sgInfo
    const timestamp = Number(new Date())
    // as the creation might be unnecessary, delayed as thunk.
    const mkEmptyShipCacheRecord = () => ({
      lastFetch: timestamp,
      sgFileName,
      sgVersion,
      files: {},
    })

    return actionCreator.swfCacheModify(
      modifyObject(
        'ship',
        modifyObject(
          mstIdX,
          _.flow(
            // (1) fill with empty record if it's missing
            (shipCacheRecord = mkEmptyShipCacheRecord()) =>
              shipCacheRecord,
            /* eslint-enable indent */
            // (2) update meta
            modifyObject('lastFetch', () => timestamp),
            modifyObject('sgFileName', () => sgFileName),
            modifyObject('sgVersion', () => sgVersion),
            // (3) update files
            modifyObject(
              'files',
              modifyObject(characterId, () => fileName)
            )
          )
        )
      )
    )
  },
  swfCacheRegisterBgm: (bgmType, bgmId) => {
    const subdir = bgmType === 'port' ? 'portBgm' : 'mapBgm'
    const timestamp = Number(new Date())
    return actionCreator.swfCacheModify(
      modifyObject(
        subdir,
        modifyObject(
          bgmId,
          () => ({lastFetch: timestamp})
        )
      )
    )
  },
  subtitleModify: modifier => ({
    type: '@poi-plugin-navy-album@subtitle@Modify',
    modifier,
  }),
  gameUpdateReady: newState => ({
    type: '@poi-plugin-navy-album@gameUpdate@Ready',
    newState,
  }),
  gameUpdateModify: modifier => ({
    type: '@poi-plugin-navy-album@gameUpdate@Modify',
    modifier,
  }),
  gameUpdateNewDigest: digest =>
    actionCreator.gameUpdateModify(
      modifyObject('digest', () => digest)
    ),
  gameUpdateNewSummary: newSummary =>
    actionCreator.gameUpdateModify(
      modifyObject(
        'summary',
        oldSummary => {
          const oneWeek = 7 * 24 * 3600 * 1000
          if (
            oldSummary &&
            newSummary.lastUpdate - oldSummary.lastUpdate <= oneWeek
          ) {
            // merge summary if the change is made within a week
            const mergeSorted = (xs, ys) =>
              _.uniq([...xs, ...ys]).sort(generalComparator)
            return {
              lastUpdate: newSummary.lastUpdate,
              addedShipMstIds:
                mergeSorted(
                  oldSummary.addedShipMstIds,
                  newSummary.addedShipMstIds
                ),
              addedEquipMstIds:
                mergeSorted(
                  oldSummary.addedEquipMstIds,
                  newSummary.addedEquipMstIds
                ),
              changedShipMstIds:
                mergeSorted(
                  oldSummary.changedShipMstIds,
                  newSummary.changedShipMstIds
                ),
            }
          } else {
            return newSummary
          }
        }
      )
    ),
  ...swfCacheAC,
  ...masterAC,
}

actionCreator.requestShipGraph =
  mkRequestShipGraph(actionCreator)

const mapDispatchToProps = _.memoize(dispatch =>
  bindActionCreators(actionCreator, dispatch))

const boundActionCreators = mapDispatchToProps(store.dispatch)

const withBoundActionCreator = (func, dispatch=store.dispatch) =>
  func(mapDispatchToProps(dispatch))

const asyncBoundActionCreator = (func, dispatch=store.dispatch) =>
  dispatch(() => setTimeout(() =>
    withBoundActionCreator(func, dispatch)))

window.bac = boundActionCreators

export {
  actionCreator,
  mapDispatchToProps,
  withBoundActionCreator,
  boundActionCreators,
  asyncBoundActionCreator,
}
