/*

   this reducer is for keeping track of game updates

   - ready: whether the state is loaded

   - digest: null or an Object which is generated from `constDigestSelector`

   - summary: null or an Object

     - lastUpdate: timestamp where this summary is updated

     - addedShipMstIds & addedEquipMstIds & changedShipMstIds:

       all of above are sorted Array of master ids

   INVARIANT: when ready === true, digest must be non-null

 */
const initState = {
  summary: null,
  digest: null,
  ready: false,
}

const reducer = (state = initState, action) => {
  if (action.type === '@poi-plugin-navy-album@gameUpdate@Ready') {
    const {newState} = action
    return {
      ...state,
      ...newState,
      ready: true,
    }
  }

  if (!state.ready)
    return state

  if (action.type === '@poi-plugin-navy-album@gameUpdate@Modify') {
    const {modifier} = action
    return modifier(state)
  }
  return state
}

export { reducer }
