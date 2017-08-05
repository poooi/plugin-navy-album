import _ from 'lodash'
import { bindActionCreators } from 'redux'
import { store } from 'views/create-store'
import { modifyObject } from 'subtender'
import { readFromBufferP, extractImages } from 'swf-extract'
import {
  swfDatabaseSelector,
  indexedShipGraphInfoSelector,
} from '../selectors'

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
  requestShipGraph: mstId =>
    (dispatch, getState) => (async () => {
      const reduxState = getState()
      const {shipDb, diskFiles, fetchLocks} =
        swfDatabaseSelector(reduxState)

      if (!_.isEmpty(shipDb[mstId]))
        return
      if (!_.isEmpty(diskFiles)) {
        // TODO should load files from disk
        return
      }

      const indexedShipGraphInfo = indexedShipGraphInfoSelector(reduxState)
      // figure out path
      const graphInfo = _.get(indexedShipGraphInfo,[mstId, 'graphInfo'])
      if (!graphInfo)
        return
      const {fileName, versionStr} = graphInfo
      const path = `/kcs/resources/swf/ships/${fileName}.swf?VERSION=${versionStr}`
      // some other process is already fetching that data
      if (path in fetchLocks)
        return
      // start fetching & parsing
      dispatch(actionCreator.swfDatabaseLockPath(path))
      try {
        // TODO: use selector
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
                actionCreator.swfDatabaseInsertShipGraph({
                  mstId,
                  sgFileName: fileName,
                  sgVersion: versionStr,
                  characterId,
                  debuffFlag: false, img: encoded,
                })
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
      // TODO: try fetching debuffed
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
