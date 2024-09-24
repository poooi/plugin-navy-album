import { modifyObject } from 'subtender'

import {
  debuffInfoSelector,
  getShipImgSrcFuncSelector,
} from '../selectors'

const testImageAvailability = (url, callback) => {
  const img = new Image()
  img.onload = () => callback(true)
  img.onerror = () => callback(false)
  img.src = url
}

/*
   check availability of "debuff graph", and put result into
   <ext store>.debuffInfo
 */
const mkTouchDebuffGraph = actionCreator => (mstId, forced) =>
  (dispatch, getState) => setTimeout(() => {
    const poiState = getState()
    const debuffInfo = debuffInfoSelector(poiState)
    const getShipImgSrc = getShipImgSrcFuncSelector(poiState)

    if (!forced) {
      if (mstId in debuffInfo)
        return
    }

    /*
       we first check if normal graph is available,
       the purpose of this is to make sure that current
       network environment works for at least regular resources
       before we try to detect something that might not exist -
       we need to rule out the case in which network is not available.
     */
    const normalImgSrc = getShipImgSrc(mstId, 'banner', false, false)
    const debuffImgSrc = getShipImgSrc(mstId, 'banner', false, true)

    const handleDebuffImgAvaResult = isDebuffImgAvailable => {
      dispatch(actionCreator.debuffInfoModify(
        modifyObject(mstId, () => isDebuffImgAvailable)
      ))
    }

    testImageAvailability(
      normalImgSrc,
      isNormalImgAvailable => {
        if (isNormalImgAvailable) {
          testImageAvailability(
            debuffImgSrc,
            handleDebuffImgAvaResult
          )
        }
      }
    )
  })

export {
  mkTouchDebuffGraph,
}
