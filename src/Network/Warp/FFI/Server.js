"use strict";

exports.fromHttpServer = svr => { return svr }

exports.createServer = options => () => { 
    var server = require('http').createServer()
    server.timeout = options.timeout
    return server 
}

exports.onRequest = svr => cb => () => {
    svr.on('request', (request, response) => {
        cb(request)(response)()
    })
}

exports.onUpgrade = svr => cb => () => {
    svr.on('upgrade', (request, socket, head) => {
        cb(request)(socket)(head)()
    })
}