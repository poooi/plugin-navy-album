/*
  Deals with poi resource hacks.

  Reference: https://github.com/poooi/poi/commit/dfc9e4688d4c177db6175981f8f0aa99f3242605
 */

import { remote } from 'electron'
import path from 'path-extra'
import fs from 'fs-extra'
import url from 'url'

const { config } = window

const getCachePath = (pathname = '') => {
  const dir = config.get('poi.misc.cache.path', remote.getGlobal('DEFAULT_CACHE_PATH'))
  return path.join(dir, pathname)
}

const hackFileCache = new Map()

const findHackFilePath = (pathname = '') => {
  if (hackFileCache.has(pathname)) {
    return hackFileCache.get(pathname)
  }

  const originFilePath = getCachePath(path.join('KanColle', pathname))
  const sp = originFilePath.split('.')
  const ext = sp.pop()
  sp.push('hack')
  if (ext) {
    sp.push(ext)
  }
  const hackedFilePath = sp.join('.')
  try {
    fs.accessSync(hackedFilePath, fs.constants.R_OK)
    hackFileCache.set(pathname, hackedFilePath)
    return hackedFilePath
  } catch (_e0) {
    try {
      fs.accessSync(originFilePath, fs.constants.R_OK)
      hackFileCache.set(pathname, originFilePath)
      return originFilePath
    } catch (_e1) {
      hackFileCache.set(pathname, undefined)
      return undefined
    }
  }
}

const pathToFileURL = (filePath = '') =>
  url.pathToFileURL(filePath.split(path.sep).join(path.posix.sep))

export {
  findHackFilePath,
  pathToFileURL,
}
