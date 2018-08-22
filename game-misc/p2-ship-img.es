import { padStart } from 'lodash'

export const shipImgType = [
  'banner',
  /*
     technically exists for sunk ships (damaged only),
     but it's too unpleasant to put in the gallery.
   */
  // 'banner_g',
  'card',
  'remodel',
  'character_up',
  'character_full',
  'full',
  'supply_character',
  'album_status',
]

const map = new Map()

// Magic of tanaka
const resource = [
  6657, 5699, 3371, 8909, 7719, 6229, 5449, 8561, 2987, 5501,
  3127, 9319, 4365, 9811, 9927, 2423, 3439, 1865, 5925, 4409,
  5509, 1517, 9695, 9255, 5325, 3691, 5519, 6949, 5607, 9539,
  4133, 7795, 5465, 2659, 6381, 6875, 4019, 9195, 5645, 2887,
  1213, 1815, 8671, 3015, 3147, 2991, 7977, 7045, 1619, 7909,
  4451, 6573, 4545, 8251, 5983, 2849, 7249, 7449, 9477, 5963,
  2711, 9019, 7375, 2201, 5631, 4893, 7653, 3719, 8819, 5839,
  1853, 9843, 9119, 7023, 5681, 2345, 9873, 6349, 9315, 3795,
  9737, 4633, 4173, 7549, 7171, 6147, 4723, 5039, 2723, 7815,
  6201, 5999, 5339, 4431, 2911, 4435, 3611, 4423, 9517, 3243,
]

function createKey(t) {
  let e = 0
  if (t !== null && t !== '') {
    for (let i = 0; i < t.length; i++) {
      e += t.charCodeAt(i)
    }
  }
  return e
}

function create(id, seed) {
  const o = id.toString().match(/\d+/)
  if (o === null || o.length === 0)
    return ''
  const r = parseInt(o[0],10)
  const s = createKey(seed)
  const a = seed == null || seed.length === 0 ? 1 : seed.length
  return (17 * (r + 7) * resource[(s + r * a) % 100] % 8973 + 1e3).toString()
}

export function getShipImgPath(id, type, damaged) {
  const mapkey = [id, type, damaged].toString()
  if (map.has(mapkey)) {
    return map.get(mapkey)
  }
  if (!shipImgType.includes(type)) {
    // throw new Error('Wrong type!')
  }
  if (type === 'album_status' && damaged) {
    throw new Error('Wrong damage status!')
  }
  const ntype = type + (damaged ? '_dmg' : '')
  const seed = `ship_${ntype}`
  const cipherNum = create(id, seed)
  const padId = padStart(id, 4, '0')
  const ret = `/kcs2/resources/ship/${ntype}/${padId}_${cipherNum}.png`
  map.set(mapkey, ret)
  return ret
}
