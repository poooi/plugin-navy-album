const reducer = (state = [] , action) => {
  if (action.type === '@poi-plugin-navy-album@Msg') {
    const {msg} = action
    return [...state, msg]
  }

  return state
}

export * from './action-creator'
export { reducer }
