import { mkSimpleReducer } from './common'

const initState = {
  // activeTab: ships / equipments
  activeTab: 'ships',
  // state for ShipsAlbum
  shipsAlbum: {
    // controls how the ship list appears
    listOptions: {
      // bool
      expanded: false,
      showSides: {
        friendly: true,
        abyssal: true,
      },
      groupShipTypes: true,
      // when true, ships who are remodeled from
      // the same ship is sorted close to each other in that order.
      groupRemodels: true,
    },
  },
  // state for EquipmentsAlbum
  equipmentsAlbum: 'TODO',
}

const reducer = mkSimpleReducer(
  initState,
  '@poi-plugin-navy-album@ui@Modify')

export { reducer }
