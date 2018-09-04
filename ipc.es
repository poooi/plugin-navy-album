import { withBoundActionCreator } from './store'
import { validMasterIdSetsSelector } from './selectors'

const register = () => {
  const {ipc, remote, getStore} = window

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

export {
  register,
}
