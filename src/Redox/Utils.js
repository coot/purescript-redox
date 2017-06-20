"use strict"

exports.logValues = function(values) {
  return function() {
    console.log.apply(null, values)
  }
}

exports.formatInstant = function(i) {
  var d = new Date(i)
  return d.getHours() + ":" + d.getMinutes() + ":" + d.getSeconds() + "." + d.getMilliseconds()
}
