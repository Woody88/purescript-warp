"use strict"; 

exports.createServer = require('http').createServer

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