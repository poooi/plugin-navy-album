import { mkSimpleReducer } from './common'

const initState = {
  /*
     NOTE: all paths should begin with '/'

     - key: path
     - value: Object

       - key: characterId
       - value: srcString

   */
  db: {},
  fetchLocks: [],
}

const reducer = mkSimpleReducer(
  initState,
  '@poi-plugin-navy-album@swfDatabase@Modify')

export { reducer }
