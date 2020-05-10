"use strict"; 
const fs = require('fs')

exports.createReadStreamWithRange = filepath => start => end => () => {
  return fs.createReadStream(filepath, {start, end})
} 