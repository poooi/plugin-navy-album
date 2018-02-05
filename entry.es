import 'views/env'

import i18n2 from 'i18n-2'
import { join } from 'path-extra'
import { remote } from 'electron'
import _ from 'lodash'

const { config } = window

// eslint-disable-next-line new-cap
const i18n = new i18n2({
  locales: ['en-US', 'ja-JP', 'zh-CN', 'zh-TW', 'ko-KR'],
  defaultLocale: 'zh-CN',
  directory: join(__dirname, 'assets', 'i18n'),
  devMode: false,
  extension: '.json',
})
i18n.setLocale(window.language)

if (i18n.resources == null) {
  i18n.resources = {}
}

if (i18n.resources.__ == null) {
  i18n.resources.__ = str => str
}
if (i18n.resources.translate == null) {
  i18n.resources.translate = (_ignored, str) => str
}
if (i18n.resources.setLocale == null) {
  i18n.resources.setLocale = () => {}
}
window.i18n = i18n
try {
  // eslint-disable-next-line global-require
  require('poi-plugin-translator').pluginDidLoad()
} catch (error) {
  console.info("failed to load poi-plugin-translator")
}

window.__ = i18n.__.bind(i18n)
window.__r = i18n.resources.__.bind(i18n.resources)

// augment font size with poi zoom level
const additionalStyle = document.createElement('style')

remote.getCurrentWindow().webContents.on('dom-ready', () => {
  document.body.appendChild(additionalStyle)
})

// remember window size
window.navyAlbumWindow = remote.getCurrentWindow()

const rememberSize = _.debounce(() => {
  const b = window.navyAlbumWindow.getBounds()
  config.set('plugin.navyAlbumWindow.bounds', b)
}, 5000)

window.navyAlbumWindow.on('move', rememberSize)
window.navyAlbumWindow.on('resize', rememberSize)

require('./ui')
