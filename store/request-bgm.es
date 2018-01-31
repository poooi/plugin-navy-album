import _ from 'lodash'
import { readFromBufferP, extractSounds } from 'swf-extract'
import { writeFileSync } from 'fs-extra'

import {
  swfCacheSelector,
  serverIpSelector,
} from '../selectors'

import { getPortBgmPath, getMapBgmPath } from '../game-misc'
import { getBgmFilePath } from '../swf-cache'

const mayExtractWithLock = async context => {
  const {
    getState, actionCreators,
    path,
    dispatch,
    dataReady,
  } = context
  const reduxState = getState()
  const {fetchLocks} = swfCacheSelector(reduxState)

  // some other process is already fetching that data
  if (fetchLocks.includes(path))
    return

  // start fetching & parsing
  dispatch(actionCreators.swfCacheLockPath(path))
  try {
    const serverIp = serverIpSelector(reduxState)
    const fetched = await fetch(`http://${serverIp}${path}`)
    if (! fetched.ok)
      throw new Error('fetch failed.')
    const ab = await fetched.arrayBuffer()
    const swfData = await readFromBufferP(new Buffer(ab))
    const sounds = _.compact(await Promise.all(extractSounds(swfData.tags)))
    dataReady(sounds)
  } catch (e) {
    console.error(`error while processing ${path}`,e)
  } finally {
    // release lock
    dispatch(actionCreators.swfCacheUnlockPath(path))
  }
}

const mkRequestBgm = actionCreators => (bgmType, bgmId, forced = false) =>
  (dispatch, getState) => setTimeout(() => {
    if (bgmType !== 'port' && bgmType !== 'map') {
      return console.error(`invalid bgmType ${bgmType}`)
    }

    const poiState = getState()
    const {
      ready,
      [bgmType === 'port' ? 'portBgm' : 'mapBgm']: bgmCache,
    } = swfCacheSelector(poiState)
    if (!ready) {
      return console.error(`swfCache not ready`)
    }

    if (!forced && !_.isEmpty(bgmCache[bgmId])) {
      return
    }

    const path = (bgmType === 'port' ? getPortBgmPath : getMapBgmPath)(bgmId)
    const extractContext = {
      getState, actionCreators,
      path,
      dispatch,
      dataReady: sounds => {
        const soundInd = sounds.findIndex(s => s.soundId === 1)
        if (soundInd === -1) {
          console.warn(`resource with soundId=1 not found`)
          return
        }
        const sound = sounds[soundInd]
        const getFP = getBgmFilePath(bgmType)
        try {
          writeFileSync(getFP(bgmId), sound.mp3Data)
          dispatch(actionCreators.swfCacheRegisterBgm(bgmType, bgmId))
        } catch (e) {
          console.error(`error while writing extracted file`)
        }
      },
    }

    mayExtractWithLock(extractContext)
  })

export { mkRequestBgm }
