import { join } from 'path-extra'
import { store } from 'views/create-store'

// TODO: no longer working, investigate.
const swfExtractor = new SharedWorker(join(__dirname, 'swf-extractor.js'))

const taskContexts = new Map()
let taskCount = 0

swfExtractor.onmessage = e => {
  if (e.data.type === 'action') {
    const ctxt = taskContexts.get(e.data.taskId)
    if (e.data.actionStr === 'lock') {
      store.dispatch(ctxt.actionLock)
    }
    if (e.data.actionStr === 'unlock') {
      store.dispatch(ctxt.actionUnlock)
    }
  }
  if (e.data.type === 'data') {
    taskContexts.get(e.data.taskId).dataReady(e.data.data)
  }
  if (e.data.type === 'done') {
    taskContexts.delete(e.data.taskId)
  }
}

swfExtractor.onmessageerror = e => {
  console.error('worker error')
  console.error(e)
}

const scheduleSwfExtract = context => {
  const taskId = taskCount
  ++taskCount

  const {dataReady, actionCreator, serverIp, path, reportError} = context
  const actionLock = actionCreator.swfCacheLockPath(context.path)
  const actionUnlock = actionCreator.swfCacheUnlockPath(context.path)
  taskContexts.set(taskId, {dataReady, actionLock, actionUnlock})
  const args = {
    serverIp,
    path,
    reportError,
    taskId,
  }
  swfExtractor.postMessage(args)
}

export {
  scheduleSwfExtract,
}
