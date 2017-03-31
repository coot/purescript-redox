"use strict"

exports.eqFns = function eqFns(fn1) {
  return function(fn2) {
    return fn1 === fn2
  }
}
