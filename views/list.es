"use strict"

const {React, ReactBootstrap} = window
const {ListGroup, ListGroupItem} = ReactBootstrap

class List extends React.Component {
  onClick(id) {
    this.props.onSelect(id)
  }

	render() {
    const {list, selected} = this.props
    console.log('List', list, selected)
    return (
      <ListGroup>
      {list.map(({id, label}) =>
        <ListGroupItem key={id} active={selected === id} onClick={this.onClick.bind(this, id)}>
          {label}
        </ListGroupItem>
      )}
      </ListGroup>
    )
  }
}

export default List