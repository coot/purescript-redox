"use strict"

exports.logValues = function(values) {
  return function() {
    console.log.apply(null, values)
  }
}

exports.formatInstant = function(i) {
  var d = new Date(i)
  var h = d.getHours()
  if (h < 10)
    h = "0" + h.toString()
  else
    h = h.toString()
  var m = d.getMinutes()
  if (m < 10)
    m = "0" + m.toString()
  else
    m = m.toString()
  var s = d.getSeconds()
  if (s < 10)
    s = "0" + s.toString()
  else
    s = s.toString()
  var mm = d.getMilliseconds()
  if (mm < 10)
    mm = "00" + mm.toString()
  else if (mm < 100)
    mm = "0" + mm.toString()
  else
    mm = mm.toString()

  return h + ":" + m + ":" + s + "." + mm
}
