# Navy Album

Navy Album is a [poi](https://github.com/poooi/poi) plugin that shows
illustrations, ship and equipment stats, and things beyond.

## Changelog

### 0.2.5

- Update WhoCallsTheFleet database

### 0.2.4

- Better handling for special CGs (again)
- Ship's internal HP range is now available in tooltip.
- Update WhoCallsTheFleet database

### 0.2.3

- Update WhoCallsTheFleet database
- Fix remodel cost for Saratoga Mk.II & Saratoga Mk.II Mod.2
- Better handling for special CGs

### 0.2.2

- Fix a problem which causes the whole plugin unusable.

### 0.2.1

- i18n

### 0.2.0

- Improvements to "Remodels" Panel

  - wider layout
  - clicking on remodels to switch between ships

- Implemented search bar for both ships and equipments

### 0.1.1

- "New CGs" => "Updated CGs"
- Fix torpedo stats not being displayed correctly

### 0.1.0

- Fix a problem that blueprints and catapults are not correctly displayed
  for ships that have cyclic remodel chains.
- Now audios are only loaded when start playing
- The color for max comsumption only changes when Level > 99
- Show after-marriage max HP when Level > 99
- Slightly reduce package size (using rc-slider dist)

## Features

### Ship Viewer

Shows ship stats and remodel-related information.

![](docs/ship-viewer-intro.jpg)

### Remodel Switcher

Click on header labels to switch between remodels of the same ship quickly.

![](docs/header1.jpg)

This also works on some abyssal ships, if they have a "debuffed" form.

![](docs/header2.jpg)

### Info Tab

A level slider to view some level-dependent stats.

![](docs/level-slider.jpg)

### Gallery

Shows CGs extracted directly from game assets for you to view and download.

![](docs/gallery.jpg)

### Voice

Listen to their voices under every situations, downloads are also available.

![](docs/voice.jpg)

Subtitles are displayed if [poi-plugin-subtitle](https://github.com/kcwikizh/poi-plugin-subtitle)
is installed and enabled.

## Equipment Viewer

Show equipments stats

![](docs/equipments.jpg)

## (experimental) Game Update Viewer

Detect changes to game assets automatically (the screenshot is just for demonstration)

![](docs/game-update.jpg)

## Acknowledgement

- [WhoCallsTheFleet](https://github.com/Diablohu/WhoCallsTheFleet) for providing
  stock equipment and ship's hidden stats data.

- [kancolle-replay](https://github.com/KC3Kai/kancolle-replay) for providing
  abyssal stats.
