// NOTE: use `npm run update-db` instead of executing this file.
const fs = require('fs-extra')
const request = require('request-promise-native')

const WHOCALLSTHEFLEET_SHIPDB =
  'https://raw.githubusercontent.com/Diablohu/WhoCallsTheFleet-DB/master/db/ships.nedb'

fs.ensureDirSync('assets/wctf')

request(WHOCALLSTHEFLEET_SHIPDB)
  .then(data => {
    fs.writeFile('assets/wctf/ships.nedb', data)
  })
