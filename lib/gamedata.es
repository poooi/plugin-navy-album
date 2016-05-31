"use strict"

const EventEmitter = require('events');

class GameData extends EventEmitter {
  constructor() {
    super()

    this.ships = []
    this.items = []

    window.addEventListener('game.response', this.gameResponse.bind(this))

    setTimeout(() => {
      const fs = require('fs-extra')
      let data = fs.readFileSync(`${__dirname}/api_start2.json`, 'utf8')
      this.gameResponse({
        detail: {
          path: '/kcsapi/api_start2',
          body: JSON.parse(data),
        },
      })
    }, 100)
  }

  gameResponse(e) {
    const req = e.detail
    const {body, postBody} = req

    if (req.path === '/kcsapi/api_start2') {
      let ships = []
      for (let ship of body.api_mst_ship) {
        ships[ship.api_id] = ship
      }
      for (let shipgraph of body.api_mst_shipgraph) {
        let ship = ships[shipgraph.api_id]
        if (ship) {
          Object.assign(ship, shipgraph)
        }
      }
      this.ships = ships

      let items = []
      for (let item of body.api_mst_slotitem) {
        items[item.api_id] = item
      }
      this.items = items

      this.emit('update')
    }
  }
}

module.exports = new GameData()
