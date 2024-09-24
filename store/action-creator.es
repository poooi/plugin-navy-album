import _ from 'lodash'
import { bindActionCreators } from 'redux'
import { store } from 'views/create-store'
import { modifyObject, generalComparator } from 'subtender'
import { actionCreators as masterAC } from './ext-root/master'
import { mkTouchDebuffGraph } from './touch-debuff-graph'
import { isAbyssalShipMstId } from '../game-misc'
import { NavyAlbum } from '../devtools'

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
                (isAbyssalShipMstId(mstId) && sv.activeTab === 'voice') ?
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
  debuffInfoModify: modifier => ({
    type: '@poi-plugin-navy-album@debuffInfo@Modify',
    modifier,
  }),
  ...masterAC,
}

actionCreator.touchDebuffGraph =
  mkTouchDebuffGraph(actionCreator)

const mapDispatchToProps = _.memoize(dispatch =>
  bindActionCreators(actionCreator, dispatch))

const boundActionCreators = mapDispatchToProps(store.dispatch)

const withBoundActionCreator = (func, dispatch=store.dispatch) =>
  func(mapDispatchToProps(dispatch))

const asyncBoundActionCreator = (func, dispatch=store.dispatch) =>
  dispatch(() => setTimeout(() =>
    withBoundActionCreator(func, dispatch)))

NavyAlbum.boundActionCreators = boundActionCreators

/*
   quick & dirty way to populate debuffInfo so user don't have
   to attempt on known abyssal ships on their own.

   execute this repeatly (hopefully once is good enought) until
   no more request is scheduled.

   then: JSON.stringify(pluginHelper.navyAlbum.getExt().debuffInfo)
 */
NavyAlbum.populateDebuffInfo = () => {
  const {getStore} = window
  const abyssalMstIds = _.flatMap(
    _.values(_.get(getStore(), ['const', '$ships'])),
    raw => isAbyssalShipMstId(raw.api_id) ? [raw.api_id] : []
  )
  const obtainedMstIds = new Set(
    _.keys(
      _.get(getStore(), ['ext', 'poi-plugin-navy-album', '_', 'debuffInfo'])
    ).map(Number)
  )

  let count = 0
  abyssalMstIds.forEach(mstId => {
    if (!obtainedMstIds.has(mstId)) {
      boundActionCreators.touchDebuffGraph(mstId, false)
      ++count
    }
  })
  /* eslint-disable no-console */
  console.log(`${count} requests scheduled`)
  /* eslint-enable no-console */
}

export {
  actionCreator,
  mapDispatchToProps,
  withBoundActionCreator,
  boundActionCreators,
  asyncBoundActionCreator,
}
