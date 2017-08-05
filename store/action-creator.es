import _ from 'lodash'
import { bindActionCreators } from 'redux'
import { store } from 'views/create-store'
import { modifyObject } from 'subtender'
import { readFromBufferP, extractImages } from 'swf-extract'
import { swfDatabaseSelector } from '../selectors'

const actionCreator = {
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
  requestSwf: path =>
    (dispatch, getState) => (async () => {
      const {db, fetchLocks} = swfDatabaseSelector(getState())
      // either it's already in db
      if (path in db)
        return

      // or some other process is fetching it
      if (path in fetchLocks)
        return

      dispatch(actionCreator.swfDatabaseLockPath(path))
      try {
        const {serverIp} = window
        const fetched = await fetch(`http://${serverIp}${path}`)
        if (! fetched.ok)
          throw new Error('fetch failed.')
        const ab = await fetched.arrayBuffer()
        const swfData = await readFromBufferP(new Buffer(ab))
        await Promise.all(
          extractImages(swfData.tags).map(async p => {
            const data = await p
            if (
              'characterId' in data &&
              ['jpeg', 'png', 'gif'].includes(data.imgType)
            ) {
              const {characterId, imgType, imgData} = data
              const encoded = `data:image/${imgType};base64,${imgData.toString('base64')}`
              dispatch(
                actionCreator.swfDatabaseInsertImage(path, characterId, encoded)
              )
            }
          })
        )
      } catch (e) {
        console.error(`error while processing ${path}`,e)
      } finally {
        // release lock
        dispatch(actionCreator.swfDatabaseUnlockPath(path))
      }
    })(),
  subtitleModify: modifier => ({
    type: '@poi-plugin-navy-album@subtitle@Modify',
    modifier,
  }),
}

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
