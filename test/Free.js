"use strict"

exports.unsafeLog = function(a) {
  return function() {
    console.log(a)
  }
}

var counter = 0
exports.getCounter = function() {
  return counter
}

exports.setCounter = function(c) {
  return function() {
    counter = c
    return {}
  }
}
