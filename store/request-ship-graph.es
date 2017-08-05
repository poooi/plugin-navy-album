import _ from 'lodash'
import { readFromBufferP, extractImages } from 'swf-extract'
import {
  swfDatabaseSelector,
  indexedShipGraphInfoSelector,
} from '../selectors'

const mayExtractWithLock = async context => {
  const {
    getState, actionCreator,
    path,
    dispatch,
    reportError,
    dataReady,
  } = context

  const reduxState = getState()
  const {fetchLocks} = swfDatabaseSelector(reduxState)

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
          dataReady(characterId, encoded)
        }
      })
    )
  } catch (e) {
    if (reportError)
      console.error(`error while processing ${path}`,e)
  } finally {
    // release lock
    dispatch(actionCreator.swfDatabaseUnlockPath(path))
  }
}

const mkRequestShipGraph = actionCreator => mstId =>
  (dispatch, getState) => (async () => {
    const reduxState = getState()
    const {shipDb, diskFiles} =
      swfDatabaseSelector(reduxState)

    if (!_.isEmpty(shipDb[mstId]))
      return

    // we don't need to check diskFilesReady,
    // assuming it's always an empty Object when diskFilesReady === false
    if (!_.isEmpty(diskFiles[mstId])) {
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

    {
      const extractContext = {
        actionCreator,
        path, getState,
        reportError: true,
        dispatch,
        dataReady: (characterId, img) => {
          dispatch(
            actionCreator.swfDatabaseInsertShipGraph({
              mstId,
              sgFileName: fileName,
              sgVersion: versionStr,
              characterId,
              debuffFlag: false, img,
            })
          )
        },
      }
      mayExtractWithLock(extractContext)
    }

    // try fetching debuffed
    if (mstId > 1500) {
      const pathDebuffed =
        `/kcs/resources/swf/ships/${fileName}_d.swf?VERSION=${versionStr}`
      const extractContext = {
        actionCreator,
        path: pathDebuffed, getState,
        reportError: false,
        dispatch,
        dataReady: (characterId, img) => {
          dispatch(
            actionCreator.swfDatabaseInsertShipGraph({
              mstId,
              sgFileName: fileName,
              sgVersion: versionStr,
              characterId,
              debuffFlag: true, img,
            })
          )
        },
      }
      mayExtractWithLock(extractContext)
    }
  })()

export { mkRequestShipGraph }
