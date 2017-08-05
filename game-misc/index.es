const interpretSpeed = speed =>
  speed === 0 ? '基地' :
  speed === 5 ? '低速' :
  speed === 10 ? '高速' :
  speed === 15 ? '高速+' :
  speed === 20 ? '最速' :
  String(speed)

const interpretRange = range =>
  range === 0 ? '無' :
  range === 1 ? '短' :
  range === 2 ? '中' :
  range === 3 ? '長' :
  range === 4 ? '超長' :
  String(range)

/* eslint-disable quote-props */
const situationTable = {
  'Intro': 1,
  'Library': 25,
  'Poke(1)': 2,
  'Poke(2)': 3,
  'Poke(3)': 4,
  'Married': 28,
  'Wedding': 24,
  'Ranking': 8,
  'Join': 13,
  'Equip(1)': 9,
  'Equip(2)': 10,
  'Equip(3)': 26,
  'Supply': 27,
  'Docking(1)': 11,
  'Docking(2)': 12,
  'Construction': 5,
  'Return': 7,
  'Sortie': 14,
  'Battle': 15,
  'Attack': 16,
  'Yasen(1)': 18,
  'Yasen(2)': 17,
  'MVP': 23,
  'Damaged(1)': 19,
  'Damaged(2)': 20,
  'Damaged(3)': 21,
  'Sunk': 22,
  'Idle': 29,
  'IdleSP': 129,
  'Repair': 6,

  'H0000': 30, 'H0100': 31, 'H0200': 32, 'H0300': 33,
  'H0400': 34, 'H0500': 35, 'H0600': 36, 'H0700': 37,
  'H0800': 38, 'H0900': 39, 'H1000': 40, 'H1100': 41,
  'H1200': 42, 'H1300': 43, 'H1400': 44, 'H1500': 45,
  'H1600': 46, 'H1700': 47, 'H1800': 48, 'H1900': 49,
  'H2000': 50, 'H2100': 51, 'H2200': 52, 'H2300': 53,
}
/* eslint-enable quote-props */

export * from './voice'
export {
  interpretSpeed,
  interpretRange,
}
