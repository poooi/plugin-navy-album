import assert from 'assert'
import { summarizeChanges } from '../../game-misc/game-update'

const spec = it

describe('game-misc', () =>
  describe('summarizeChanges', () =>
    spec('tests', () => {
      const digest1 = {
        equipMstIds: [1,2,3,41,50,51],
        shipDigests: [[1,'sg1'],[2,'sg2'],[6,'sg6'],[8,'remove']],
      }
      const digest2 = {
        equipMstIds: [1,2,3,4,41,51,6,7,8],
        shipDigests: [[1,'sg1'],[2,'sgAAA'],[3,'sg3'],[6,'sg6'],[10,'sgZZZ']],
      }

      assert.equal(summarizeChanges(digest1,digest1),null)
      assert.equal(summarizeChanges(digest2,digest2),null)

      {
        // from digest1 to digest2
        const summary = summarizeChanges(digest2,digest1)
        assert.deepEqual(summary.addedShipMstIds, [3,10])
        assert.deepEqual(summary.changedShipMstIds, [2])
        assert.deepEqual(summary.addedEquipMstIds,[4,6,7,8])
      }
      {
        // from digest2 to digest1
        const summary = summarizeChanges(digest1,digest2)
        assert.deepEqual(summary.addedShipMstIds, [8])
        assert.deepEqual(summary.changedShipMstIds, [2])
        assert.deepEqual(summary.addedEquipMstIds,[50])
      }
    })
  )
)
