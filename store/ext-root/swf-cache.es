// see ../../docs/swf-caching.md for details
const initState = {
  ship: {},
  portBgm: {},
  mapBgm: {},

  // the following props do not require persistence
  fetchLocks: [],
  ready: false,
}

const tyModify = '@poi-plugin-navy-album@swfCache@Modify'
const tyReady = '@poi-plugin-navy-album@swfCache@Ready'

const reducer = (state = initState, action) => {
  if (action.type === tyReady) {
    const {newState} = action
    return {
      ...state,
      ...(newState || {}),
      ready: true,
    }
  }

  if (!state.ready)
    return state

  if (action.type === tyModify) {
    const {modifier} = action
    return modifier(state)
  }

  return state
}

const actionCreators = {
  swfCacheReady: newState => ({
    type: tyReady,
    newState,
  }),
  swfCacheModify: modifier => ({
    type: tyModify,
    modifier,
  }),
}

export {
  initState,
  reducer,
  tyModify, tyReady,
  actionCreators,
}
