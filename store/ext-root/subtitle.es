import { mkSimpleReducer } from './common'

const initState = {}

const reducer = mkSimpleReducer(
  initState,
  '@poi-plugin-navy-album@subtitle@Modify')

export { reducer }
