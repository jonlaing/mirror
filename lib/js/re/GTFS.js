// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';

var GtfsRealtimeBindings = require("gtfs-realtime-bindings");

var FeedMessage = /* module */[];

function decode(res) {
  return GtfsRealtimeBindings.FeedMessage.decode(res);
}

exports.FeedMessage = FeedMessage;
exports.decode      = decode;
/* gtfs-realtime-bindings Not a pure module */