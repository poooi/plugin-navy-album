const { readFromBufferP, extractImages } = require('swf-extract')

const postAction = (actionStr, taskId) => postMessage({type: 'action', actionStr, taskId})
const postData = (data, taskId) => postMessage({type: 'data', data, taskId})

onmessage = async event => {
  const {
    serverIp,
    path,
    reportError,
    taskId,
  } = event.data
  postAction('lock', taskId)
  try {
    const fetched = await fetch(`http://${serverIp}${path}`)
    if (!fetched.ok)
      throw new Error('fetch failed.')
    const ab = await fetched.arrayBuffer()
    const swfData = await readFromBufferP(Buffer.from(ab))
    await Promise.all(
      extractImages(swfData.tags).map(async p => {
        const data = await p
        if (
          'characterId' in data &&
          ['jpeg', 'png', 'gif'].includes(data.imgType)
        ) {
          postData(data, taskId)
        }
      })
    )
  } catch (e) {
    if (reportError)
      console.error(`error while processing ${path}`, e)
  } finally {
    // release lock
    postAction('unlock', taskId)
  }
}
