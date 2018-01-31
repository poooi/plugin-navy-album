import _ from 'lodash'
import { writeFileSync } from 'fs'
import { readFromBufferP, extractImages } from 'swf-extract'
import {
  swfCacheSelector,
  indexedShipGraphInfoSelector,
  serverIpSelector,
} from '../selectors'

import { getShipFilePath } from '../swf-cache'

/*
   INVARIANT: swfCache is already when this function is called
 */
const mayExtractWithLock = async context => {
  const {
    getState, actionCreator,
    path,
    dispatch,
    reportError,
    dataReady,
  } = context

  const reduxState = getState()
  const {fetchLocks} = swfCacheSelector(reduxState)

  // some other process is already fetching that data
  if (fetchLocks.includes(path))
    return

  // start fetching & parsing
  dispatch(actionCreator.swfCacheLockPath(path))
  try {
    const serverIp = serverIpSelector(reduxState)
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
          dataReady(data)
        }
      })
    )
  } catch (e) {
    if (reportError)
      console.error(`error while processing ${path}`,e)
  } finally {
    // release lock
    dispatch(actionCreator.swfCacheUnlockPath(path))
  }
}

const mkRequestShipGraph = actionCreator => (mstId, forced = false) =>
  (dispatch, getState) => setTimeout(() => {
    const reduxState = getState()
    const {ready, ship} = swfCacheSelector(reduxState)
    if (!ready) {
      return console.error(`swfCache not ready`)
    }

    const indexedShipGraphInfo = indexedShipGraphInfoSelector(reduxState)
    // figure out path
    const graphInfo = _.get(indexedShipGraphInfo,[mstId, 'graphInfo'])
    if (!graphInfo)
      return
    const {fileName, versionStr} = graphInfo

    // determine whether current cache is sufficient and no further processing is required
    if (!forced) {
      const shipInfo = ship[mstId]
      if (
        !_.isEmpty(shipInfo) &&
        shipInfo.sgVersion === versionStr &&
        shipInfo.sgFileName === fileName
      ) {
        return
      }
    }

    const path = `/kcs/resources/swf/ships/${fileName}.swf?VERSION=${versionStr}`

    {
      const extractContext = {
        actionCreator,
        path, getState,
        reportError: true,
        dispatch,
        dataReady: data => {
          const {characterId, imgType, imgData} = data
          try {
            const fName = `${characterId}.${imgType}`
            writeFileSync(getShipFilePath(mstId)(fName), imgData)
            const sgInfo = {
              mstIdX: mstId,
              sgFileName: graphInfo.fileName,
              sgVersion: graphInfo.versionStr,
              characterId,
              fileName: fName,
            }
            dispatch(actionCreator.swfCacheRegisterShipGraph(sgInfo))
          } catch (e) {
            console.error(`error while writing extracted file`)
            console.error(e)
          }
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
        dataReady: data => {
          const {characterId, imgType, imgData} = data
          try {
            const fName = `${characterId}.${imgType}`
            const mstIdX = `${mstId}_d`
            writeFileSync(getShipFilePath(mstIdX)(fName), imgData)
            const sgInfo = {
              mstIdX,
              sgFileName: graphInfo.fileName,
              sgVersion: graphInfo.versionStr,
              characterId,
              fileName: fName,
            }
            dispatch(actionCreator.swfCacheRegisterShipGraph(sgInfo))
          } catch (e) {
            console.error(`error while writing extracted file`)
          }
        },
      }
      mayExtractWithLock(extractContext)
    }
  })

export { mkRequestShipGraph }
