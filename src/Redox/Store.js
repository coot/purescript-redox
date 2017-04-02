"use strict"

function Store(state) {
  this.state = state
  this.subscriptions = []
}
Store.prototype.subscribe = function(fn) {
  this.subscriptions.push(fn)
  return this.subscriptions.length - 1
}
Store.prototype.unsubscribe = function(idx) {
  this.subscriptions.splice(idx, 1)
  return {}
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

exports._subscribe = function(store) {
  return function(fn) {
    return function() {
      return store.subscribe(fn)
    }
  }
}

exports._unsubscribe = function(store) {
  return function(idx) {
    return function() {
      store.unsubscribe(idx)
      return {}
    }
  }
}

exports.mapStore = function(fn) {
  return function(store) {
    return function() {
      store.state = fn(store.state)
      return store
    }
  }
}
