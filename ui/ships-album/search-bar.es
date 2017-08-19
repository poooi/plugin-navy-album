import React, { Component } from 'react'
import {
  FormControl, Button,
} from 'react-bootstrap'
import FontAwesome from 'react-fontawesome'
import { PTyp } from '../../ptyp'

class SearchBar extends Component {
  static propTypes = {
    style: PTyp.object.isRequired,
    value: PTyp.string.isRequired,
    changeValue: PTyp.func.isRequired,
  }

  constructor(props) {
    super(props)
    this.state = {
      value: props.value,
    }
  }

  componentWillReceiveProps(nextProps) {
    if (this.props.value !== nextProps.value)
      this.setState({value: nextProps.value})
  }

  handleChangeValue = e => {
    const {value} = e.target
    this.setState({value})
  }

  render() {
    const {style} = this.props
    return (
      <div style={{
        display: 'flex',
        alignItems: 'baseline',
        ...style,
      }}>
        <FormControl
          style={{flex: 1}}
          type="text"
          placeholder="Search ..."
          value={this.state.value}
          onChange={this.handleChangeValue}
        />
        <Button
          style={this.state.value ? {} : {display: 'none'}}
          onClick={() => this.setState({searchText: ''})}
          bsSize="xsmall">
          <FontAwesome name="close" />
        </Button>
      </div>
    )
  }
}

export { SearchBar }
