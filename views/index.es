"use strict"

const {React, ReactBootstrap} = window
const {Table, Tabs, Tab, Col} = ReactBootstrap
const List = require('./list')

const GameData = require('../lib/gamedata')

class ShipDetail extends React.Component {
  render() {
    <Table>
    </Table>
  }
}

class MainArea extends React.Component {
  constructor() {
    super()
    GameData.addListener('update', () => this.forceUpdate())
  }

  render() {
    return (
      <Tabs id="main">
        <Tab eventKey={1} title={__('Ships')}>
          <Col xs={3}>
            <List list={GameData.ships.map((ship) => {
              return {
                id: ship.api_id,
                label: ship.api_name,
              }
            })} onSelect={console.log.bind(console)} />
          </Col>
          <Col xs={9}>
            asdasdasdasdasdasdasssssssssssssssshdfhf
          </Col>
        </Tab>
        <Tab eventKey={2} title={__('Items')}>
        </Tab>
      </Tabs>
    )
  }
}

export default MainArea