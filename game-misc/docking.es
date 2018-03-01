// reference: http://kancolle.wikia.com/wiki/Docking
/* eslint-disable indent */
/*
const getDockingFactor = stype =>
  [9, 10, 11, 18, 19].includes(stype) ? 2 :
  [5, 6, 8, 7, 20].includes(stype) ? 1.5 :
  [2, 3, 4, 21, 16, 14, 17, 22].includes(stype) ? 1 :
  [13, 1].includes(stype) ? 0.5 : NaN
*/
/* eslint-enable indent */

/*
   computeDockingTimePerHp expects a function getDockingFactor,
   which when given a ship type (in number), returns the factor.
   see above for the original implementation, but it turns out
   `getDockingFactor` can actually be derived from store,
   so we'll do that instead.

   please use getDockingFactorFuncSelector from selectors/game-consts.es
 */
const computeDockingTimePerHpWith = getDockingFactor => (stype,level) => {
  const baseTime = level <= 11 ?
    level*10 :
    level*5+Math.floor(Math.sqrt(level-11))*10+50

  return baseTime*getDockingFactor(stype)
}

const computeHealthState = (now,max) => {
  if (now === max)
    return 'full'
  const percent = now/max * 100
  /* eslint-disable indent */
  return percent <= 25 ? 'taiha' :
    percent <= 50 ? 'chuuha' :
    percent <= 75 ? 'shouha' :
    'normal'
  /* eslint-enable indent */
}

const healthStateColors = {
  full: '#4CAF50',
  normal: '#4CAF50',
  shouha: '#FBC02D',
  chuuha: '#F57C00',
  taiha: '#D50000',
}


export {
  computeDockingTimePerHpWith,
  healthStateColors,
  computeHealthState,
}
