import { withBoundActionCreator } from './store'
import { validMasterIdSetsSelector } from './selectors'

const register = () => {
  const {ipc, getStore} = window

  const services = withBoundActionCreator(bac => ({
    showShip: mstIdRaw => {
      const mstId = Number(mstIdRaw)
      const {shipIdSet} = validMasterIdSetsSelector(getStore())
      if (shipIdSet.has(mstId)) {
        bac.uiSwitchShip(mstId)
      } else {
        console.error(`ship id ${mstIdRaw} is not valid`)
      }
    },
    showEquip: mstIdRaw => {
      const mstId = Number(mstIdRaw)
      const {equipIdSet} = validMasterIdSetsSelector(getStore())
      if (equipIdSet.has(mstId)) {
        bac.uiSwitchShip(mstId)
      } else {
        console.error(`equip id ${mstIdRaw} is not valid`)
      }
    },
  }))

  ipc.register('NavyAlbum', services)

  const unregister = () =>
    ipc.unregisterAll('NavyAlbum')
  return unregister
}

let unregisterFunc = null

const registerIpc = () => {
  if (unregisterFunc !== null) {
    console.warn(`unregisterFunc should be null while getting ${unregisterFunc}`)
    if (typeof unregisterFunc === 'function') {
      try {
        unregisterFunc()
      } finally {
        unregisterFunc = null
      }
    }
  }
  unregisterFunc = register()
}

const unregisterIpc = () => {
  if (typeof unregisterFunc !== 'function') {
    console.error(`unexpected unregisterIpc value: ${unregisterFunc}`)
  } else {
    try {
      unregisterFunc()
    } finally {
      unregisterFunc = null
    }
  }
}

export {
  registerIpc, unregisterIpc,
}
