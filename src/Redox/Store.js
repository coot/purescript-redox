"use strict"

function Store(state) {
  this.state = state
  this.subscriptions = []
}
Store.prototype.subscribe = function(fn) {
  this.subscriptions.push(fn)
}
Store.prototype.unsubscribe = function(fn) {
  this.subscriptions = this.subscriptions.filter(function(fn_) {return fn_ !== fn})
}

exports.mkStore = function mkStore(state) {
  return function() {
    return new Store(state)
  }
}

exports.getState = function(store) {
  return function() {
    return store.state
  }
}

exports.setState = function(store) {
  return function(state) {
    return function() {
      store.state = state
      return store
    }
  }
}

exports.getSubs = function(store) {
  return function() {
    return store.subscriptions
  }
}

exports.subscribe = function(store) {
  return function(fn) {
    return function() {
      store.subscribe(fn)
      return {}
    }
  }
}

exports.unsubscribe = function(store) {
  return function(fn) {
    return function() {
      store.unsubscribe(fn)
      return {}
    }
  }
}

exports.mapStore = function(fn) {
  return function(store) {
    store.state = fn(store.state)
    return store
  }
}
