import _ from 'lodash'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import { join } from 'path-extra'
import { readJson } from 'fs-extra'
import { observer } from 'redux-observers'
import {
  poiConfigSelector,
} from '../selectors'
import {
  withBoundActionCreator,
} from '../store'

const subtitleAvailabilitySelector = createSelector(
  poiConfigSelector,
  c => _.get(c,'plugin.poi-plugin-subtitle.enable') === true
)

// since only zh-CN and ja-JP are available
const subLangSelector = createSelector(
  poiConfigSelector,
  c => {
    const lang = _.get(c,'poi.language')
    return lang === 'zh-CN' ? lang : 'ja-JP'
  }
)

const asyncLoadSubtitle = async (dispatch, lang) => {
  const {PLUGIN_PATH} = window
  const fileName =
    join(PLUGIN_PATH, 'node_modules', 'poi-plugin-subtitle', 'data', `${lang}.json`)
  try {
    const data = await readJson(fileName)
    withBoundActionCreator(
      ({subtitleModify}) => subtitleModify(() => data),
      dispatch
    )
  } catch (e) {
    console.error(`failed to load subtitles from ${fileName}`, e)
  }
}

const subtitleLoader = observer(
  createStructuredSelector({
    available: subtitleAvailabilitySelector,
    lang: subLangSelector,
  }),
  (dispatch, cur, prev) => {
    if (! cur.available)
      return

    if (
      typeof prev === 'undefined' ||
      prev.lang !== cur.lang
    ) {
      asyncLoadSubtitle(dispatch, cur.lang)
    }
  },
  {skipInitialCall: false}
)

export { subtitleLoader }
