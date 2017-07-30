import _ from 'lodash'
import { bindActionCreators } from 'redux'
import { store } from 'views/create-store'

const actionCreator = {
  sendMsg: msg => ({
    type: '@poi-plugin-navy-album@Msg',
    msg,
  }),
}

const mapDispatchToProps = _.memoize(dispatch =>
  bindActionCreators(actionCreator, dispatch))

const withBoundActionCreator = (func, dispatch=store.dispatch) =>
  func(mapDispatchToProps(dispatch))

const asyncBoundActionCreator = (func, dispatch=store.dispatch) =>
  dispatch(() => setTimeout(() =>
    withBoundActionCreator(func, dispatch)))

export {
  actionCreator,
  mapDispatchToProps,
  withBoundActionCreator,
  asyncBoundActionCreator,
}
