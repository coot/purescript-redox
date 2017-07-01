"use strict"

exports.logValues = function(values) {
  return function() {
    console.log.apply(null, values)
  }
}

exports.nowJSDate = function() {
  return new Date()
}

exports.getLocale = function(window) {
  return function() {
    return window.navigator.language
  }
}
