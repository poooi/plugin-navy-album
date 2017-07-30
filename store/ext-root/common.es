// make a simple reducer that only accepts a modify action
const mkSimpleReducer = (initStateValOrThunk, modifyActionType) => {
  const getInitState =
    typeof initStateValOrThunk === 'function' ?
      initStateValOrThunk :
      () => initStateValOrThunk
  // reducer template
  return (state = getInitState(), action) => {
    if (action.type === modifyActionType) {
      const {modifier} = action
      return modifier(state)
    }
    return state
  }
}

export { mkSimpleReducer }
