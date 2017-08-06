import _ from 'lodash'

const summarizeChanges = (curDigest, prevDigest) => {
  const lastUpdate = Number(new Date())
  const addedEquipMstIds = curDigest.equipMstIds.filter(
    mstId => !prevDigest.equipMstIds.includes(mstId))

  const indexedPrevShipDigests = _.fromPairs(
    prevDigest.shipDigests)

  const [addedShipDigests,existingShipDigests] =
    _.partition(
      curDigest.shipDigests,
      ([mstId]) => !(mstId in indexedPrevShipDigests)
    )

  const addedShipMstIds = addedShipDigests.map(([mstId]) => mstId)
  const changedShipMstIds = _.flatMap(
    existingShipDigests,
    ([mstId,curGraphDigest]) => {
      const prevGraphDigest = indexedPrevShipDigests[mstId]
      return prevGraphDigest !== curGraphDigest ? [mstId] : []
    }
  )
  const prepared = {
    lastUpdate,
    addedShipMstIds,
    addedEquipMstIds,
    changedShipMstIds,
  }

  if (
    addedShipMstIds.length > 0 ||
    addedEquipMstIds.length > 0 ||
    changedShipMstIds.length > 0
  ) {
    return prepared
  } else {
    return null
  }
}

export { summarizeChanges }
