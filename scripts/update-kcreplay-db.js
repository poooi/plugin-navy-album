// NOTE: use `npm run update-kcreplay` instead of executing this file.
const request = require('request-promise-native')
const localeval = require('localeval')
const _ = require('lodash')
const { writeJsonSync } = require('fs-extra')

request('https://raw.githubusercontent.com/KC3Kai/kancolle-replay/master/js/kcSHIPDATA.js')
  .then(data => {
    // redo serialization to destroy non-serializable stuff like functions.
    const raw = JSON.stringify(localeval(`${data}; SHIPDATA`))
    const kcReplayData = JSON.parse(raw)

    const resultObj = _.fromPairs(
      _.flatMap(
        _.toPairs(kcReplayData),
        ([mstIdStr, shipRaw]) => {
          const mstId = Number(mstIdStr)
          if (! _.isInteger(mstId) || mstId <= 1500)
            return []
          return [
            [
              // key
              mstIdStr,
              // value
              _.fromPairs(
                'HP FP AR TP EV AA ASW SPD LOS RNG LUK SLOTS EQUIPS'
                  .split(' ').map(propName =>
                    [propName, shipRaw[propName]])),
            ],
          ]
        }))

    writeJsonSync('assets/abyssal.json', resultObj)
  })
