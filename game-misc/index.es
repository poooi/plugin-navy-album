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

export {
  interpretSpeed,
  interpretRange,
}

export * from './voice'
export * from './game-update'
export * from './docking'
export * from './magic'
