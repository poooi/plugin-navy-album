/*
   responsible for maintaining disk files.

   see ../docs/swf-caching.md for details

   - when shipDb change happens, it's saved as a file
   - when file saving is done, `diskFiles` is maintained
     accordingly, which is then also saved as index.json
*/
import _ from 'lodash'
import shallowEqual from 'shallowequal'
import { observer } from 'redux-observers'

import { swfDatabaseSelector } from '../selectors'
import { withBoundActionCreator } from '../store'
import { writeCacheFile, writeIndexFile } from '../swf-cache'

const debouncedWriteCacheFile =
  _.memoize(dispatch =>
    _.memoize(mstId =>
      _.debounce(
        async shipRecord => {
          try {
            writeCacheFile(mstId,shipRecord)
            withBoundActionCreator(
              ({swfDatabaseDiskFileUpdate}) =>
                swfDatabaseDiskFileUpdate(mstId,shipRecord),
              dispatch
            )
          } catch (e) {
            console.error('failed while writing cache file', e)
          }
        },
        1000
      )
    )
  )

const swfCacheUpdater = observer(
  state => {
    const {shipDb, diskFiles, diskFilesReady} =
      swfDatabaseSelector(state)
    return {shipDb, diskFiles, diskFilesReady}
  },
  (dispatch, cur, prev) => {
    if (!cur.diskFilesReady)
      return

    const curDb = cur.shipDb
    const prevDb = prev.shipDb
    // detect shipDb changes
    const addedMstIds = []
    const removedMstIds = []
    const modifiedMstIds = []

    Object.keys(curDb).map(mstIdStr => {
      const mstId = Number(mstIdStr)
      if (mstIdStr in prevDb) {
        if (! shallowEqual(curDb[mstIdStr],prevDb[mstIdStr])) {
          modifiedMstIds.push(mstId)
        }
      } else {
        addedMstIds.push(mstId)
      }
    })
    Object.keys(prevDb).map(mstIdStr => {
      if (!(mstIdStr in curDb)) {
        removedMstIds.push(Number(mstIdStr))
      }
    })

    const debouncedWrite = debouncedWriteCacheFile(dispatch);
    // xxxMstIds prepared.
    [...addedMstIds, ...modifiedMstIds].map(mstId =>
      debouncedWrite(mstId)(curDb[mstId])
    )

    // TODO: removal
  }
)

const debouncedWriteIndexFile = _.debounce(
  diskFiles => {
    try {
      writeIndexFile(diskFiles)
    } catch (e) {
      console.error('failed while updating index file', e)
    }
  },
  500
)

const swfCacheIndexFileUpdater = observer(
  state => {
    const {diskFiles, diskFilesReady} =
      swfDatabaseSelector(state)
    return {diskFiles, diskFilesReady}
  },
  (_dispatch, cur, prev) => {
    if (!cur.diskFilesReady)
      return
    if (!shallowEqual(cur.diskFiles,prev.diskFiles)) {
      // change detected, updating index file
      debouncedWriteIndexFile(cur.diskFiles)
    }
  }
)

export {
  swfCacheUpdater,
  swfCacheIndexFileUpdater,
}
