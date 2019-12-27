"use strict";

exports.httpVersionMajor = (request) => {
    return request.httpVersionMajor;
};

exports.httpVersionMinor = (request) => {
    return request.httpVersionMinor;
};