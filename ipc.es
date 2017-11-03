import { withBoundActionCreator } from './store'

const register = () => {
  const {ipc, remote} = window

  const services = withBoundActionCreator(bac => ({
    showShip: mstId => {
      bac.uiSwitchShip(mstId)
      remote.getCurrentWindow().show()
    },
    showEquip: mstId => {
      bac.uiSwitchShip(mstId)
      remote.getCurrentWindow().show()
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
