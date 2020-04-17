"use strict";


exports.end = httpres => data => () => {
    httpres.end(data)
}