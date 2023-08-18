import _ from 'lodash'
import React, { Component } from 'react'
import { FormControl, Button } from 'react-bootstrap'
import FontAwesome from 'react-fontawesome'
import { PTyp } from '../ptyp'

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

  debouncedChangeValue = _.debounce(
    value =>
      // delaying accessing props.changeValue until the time is right
      this.props.changeValue(value),
    500)

  handleChangeValue = e => {
    const {value} = e.target
    this.setState({value})
    this.debouncedChangeValue(value)
  }

  handleClearValue = () => {
    this.setState({value: ''})
    this.debouncedChangeValue('')
  }

  render() {
    const {style} = this.props
    const {__} = window.i18n["poi-plugin-navy-album"]
    return (
      <div style={{
        display: 'flex',
        alignItems: 'baseline',
        ...style,
      }}>
        <FormControl
          style={{flex: 1}}
          type="text"
          placeholder={__('Search')}
          value={this.state.value}
          onChange={this.handleChangeValue}
        />
        <Button
          style={this.state.value ? {} : {display: 'none'}}
          onClick={this.handleClearValue}
          bsSize="xsmall">
          <FontAwesome name="close" />
        </Button>
      </div>
    )
  }
}

export { SearchBar }
