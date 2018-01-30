import { mkSimpleReducer } from 'subtender'

// see ../../docs/swf-caching.md for details
const initState = {
  ship: {},
  portBgm: {},
  mapBgm: {},
  fetchLocks: [],

  // added by mkSimpleReducer
  // ready: false,
}

const tyModify = '@poi-plugin-navy-album@swfCache@Modify'
const tyReady = '@poi-plugin-navy-album@swfCache@Ready'

const reducer = mkSimpleReducer(
  initState,
  tyModify,
  tyReady,
)

export {
  initState,
  reducer,
  tyModify, tyReady,
}
