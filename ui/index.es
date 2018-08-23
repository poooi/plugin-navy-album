// import _ from 'lodash'
import React from 'react'

import { NavyAlbum } from './navy-album'

// const {$} = window

// $('#fontawesome-css')
//  .setAttribute('href', require.resolve('font-awesome/css/font-awesome.css'))

// extendReducer('poi-plugin-navy-album', reducer)

/*
   TODO

   - collapsible type group?
   - ship album
       - handle special CGs
   - special CG handling: check shipgraph as well
   - non-forced batch request for game update tab
   - settings tab
      - loop toggle

 */

const NavyAlbumRoot = _props => (
  <div
    style={{margin: "0 1%", minWidth: 600}}
    className="navy-album-main">
    <NavyAlbum />
  </div>
)

export { NavyAlbumRoot }
