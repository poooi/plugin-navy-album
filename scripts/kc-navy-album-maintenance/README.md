Few environment variables required to run the script:

## `NAVY_ALBUM_REPO`

local path to project home.

## `REMODEL_COST_CALCULATOR`

Only needed for `build-remodel-useitem-consumption`. Path to a binary
that can take JSON input from stdin and generate `remodel-info-useitem.json` for us.

Could be a Bash script like this:

```bash
#!/bin/bash

pushd ~/some/calculator >/dev/null
exec node index.js
```
