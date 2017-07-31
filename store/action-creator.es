import _ from 'lodash'
import { bindActionCreators } from 'redux'
import { store } from 'views/create-store'
import { modifyObject } from 'subtender'
import { readFromBufferP, extractImages } from 'swf-extract'
import { swfDatabaseSelector } from '../selectors'

const actionCreator = {
  uiModify: modifier => ({
    type: '@poi-plugin-navy-album@ui@Modify',
    modifier,
  }),
  swfDatabaseModify: modifier => ({
    type: '@poi-plugin-navy-album@swfDatabase@Modify',
    modifier,
  }),
  requestSwf: path =>
    (dispatch, getState) => (async () => {
      const {db, fetchLocks} = swfDatabaseSelector(getState())
      if (
        // either it's already in db
        path in db ||
        // or some other process is fetching it
        path in fetchLocks
      )
        return

      // register a lock
      dispatch(actionCreator.swfDatabaseModify(
        modifyObject(
          'fetchLocks', fl => [...fl, path]
        )
      ))

      try {
        const {serverIp} = window
        const fetched = await fetch(`http://${serverIp}${path}`)
        if (! fetched.ok)
          throw new Error('fetch failed.')
        const ab = await fetched.arrayBuffer()
        const swfData = await readFromBufferP(new Buffer(ab))
        extractImages(swfData.tags).map(p => p.then(data => {
          if (
            'characterId' in data &&
            ['jpeg', 'png', 'gif'].includes(data.imgType)
          ) {
            const {characterId, imgType, imgData} = data
            const encoded =
              `data:image/${imgType};base64,${imgData.toString('base64')}`

            dispatch(actionCreator.swfDatabaseModify(
              modifyObject(
                'db',
                modifyObject(
                  path,
                  (record = {}) =>
                    modifyObject(
                      characterId,
                      () => encoded
                    )(record)
                )
              )
            ))
          }
        }))
      } catch (e) {
        console.error(`error while processing ${path}`,e)
      } finally {
        // release lock
        dispatch(actionCreator.swfDatabaseModify(
          modifyObject(
            'fetchLocks', fl => fl.filter(p => p !== path)
          )
        ))
      }
    })(),
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
