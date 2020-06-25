"use strict"

exports.createServer = () => {
  return require('http').createServer()
}

exports.onRequest = server => f => () => {
  server.on('request', (req, res) => {
    f(req)(res)()
  })
}

exports.listen = server => port => hostname => action => () => {
  server.listen(port, hostname, action)
}