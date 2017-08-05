import { mkSimpleReducer } from './common'

const initState = {
  /*
     NOTE: all paths should begin with '/'

     - key: path
     - value: Object

       - key: characterId
       - value: srcString

   */
  // see ../../docs/swf-caching.md for details
  shipDb: {},
  fetchLocks: [],
  diskFiles: {},
  // TODO: false
  diskFilesReady: true,
}

const reducer = mkSimpleReducer(
  initState,
  '@poi-plugin-navy-album@swfDatabase@Modify')

export { reducer }
