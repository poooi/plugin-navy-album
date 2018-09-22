/*
   debuffInfo is simply an Object
   whose keys are mstIds and values booleans.
   to indicate whether a "isBossDamaged" graph exists
 */
const initState = {}

const reducer = (state = initState, action) => {
  if (action.type === '@poi-plugin-navy-album@debuffInfo@Modify') {
    const {modifier} = action
    return modifier(state)
  }
  return state
}

export { reducer }
