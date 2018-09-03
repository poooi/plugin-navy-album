import React from 'react'
import { join } from 'path-extra'
import { NavyAlbum } from './navy-album'

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
    className="navy-album-main"
  >
    <link
      rel="stylesheet"
      href={join(__dirname, '..', 'assets', 'rc-slider.min.css')}
    />
    <link
      rel="stylesheet"
      href={join(__dirname, '..', 'assets', 'navy-album.css')}
    />
    <NavyAlbum />
  </div>
)

export { NavyAlbumRoot }
