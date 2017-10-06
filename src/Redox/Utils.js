"use strict"

exports.logValues = function(values) {
  return function() {
    console.log.apply(null, values)
  }
}
