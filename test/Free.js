"use strict"

exports.unsafeLog = function(a) {
  return function() {
    console.log(a)
  }
}
