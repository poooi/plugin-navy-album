import { ensureDirSync, readJsonSync, writeJsonSync } from 'fs-extra'
import { join } from 'path-extra'

// state persistence: for now only extStore.ui is kept and restored at runtime.
const stateToPState = ({ui}) => ({
  ui,
  $dataVersion: 'initial',
})

const getPStateFilePath = () => {
  const {APPDATA_PATH} = window
  const path = join(APPDATA_PATH,'navy-album')
  ensureDirSync(path)
  return join(path,'p-state.json')
}

const savePState = pState => {
  const path = getPStateFilePath()
  try {
    writeJsonSync(path,pState)
  } catch (err) {
    console.error('Error while writing to p-state file', err)
  }
}

const updatePState = oldPState => {
  if (oldPState.$dataVersion === 'initial')
    return oldPState

  throw new Error('failed to update the config')
}

const loadPState = () => {
  try {
    return updatePState(readJsonSync(getPStateFilePath()))
  } catch (err) {
    if (err.syscall !== 'open' || err.code !== 'ENOENT') {
      console.error('Error while loading config', err)
    }
  }
  return null
}

export { stateToPState, savePState, loadPState }
