[@bs.val] external bufferFrom : Bs_fetch.arrayBuffer => Node.Buffer.t = "Buffer.from";

open Js.Promise;

open Utils.Option;

type stopTimeEvent = {time: option(int)};

type vehicleDescriptor = {
  id: option(string),
  label: option(string),
  licensePlate: option(string)
};

type scheduleRelationship =
  | Scheduled
  | Added
  | Unscheduled
  | Canceled;

type stopTimeUpdate = {
  stopId: option(string),
  arrival: option(stopTimeEvent),
  departure: option(stopTimeEvent)
};

type tripDescriptor = {
  id: option(string),
  routeId: option(string),
  directionId: option(int),
  startDate: option(string),
  scheduleRelationship: option(scheduleRelationship)
};

type tripUpdate = {
  trip: tripDescriptor,
  stopTimeUpdate: option(list(stopTimeUpdate)),
  timestamp: option(int),
  delay: option(int)
};

type vehicleStopStatus =
  | IncomingAt
  | StoppedAt
  | InTransitTo;

type vehiclePosition = {
  trip: option(tripDescriptor),
  currentStopSequence: option(int),
  stopId: option(string),
  currentStatus: option(vehicleStopStatus),
  timestamp: option(int)
};

type timeRange = {
  startTime: option(int),
  endTime: option(int)
};

type entitySelector = {trip: option(tripDescriptor)};

type alert = {
  informedEntity: list(entitySelector),
  headerText: string
};

type feedEntity = {
  id: string,
  tripUpdate: option(tripUpdate),
  vehicle: option(vehiclePosition),
  alert: option(alert)
};

type feedMessage = {entity: list(feedEntity)};

module Lenses = {
  open Lens;
  let entity = make(({entity}) => entity, (e, fm) => {entity: e});
  let tripUpdate = make(({tripUpdate}) => tripUpdate, (u, o) => {...o, tripUpdate: u});
  let alert =
    make(({alert}) => alert, (a, o) => {...o, alert: a})
    >>- optional({informedEntity: [], headerText: ""});
  let stopTimeUpdate =
    make(({stopTimeUpdate}) => stopTimeUpdate, (u, o) => {...o, stopTimeUpdate: u})
    >>- optional([]);
};

module Decode = {
  let stopTimeEvent = (json) => Json.Decode.{time: json |> optional(field("time", int))};
  let vehicleDescriptor = (json) =>
    Json.Decode.{
      id: json |> optional(field("id", string)),
      label: json |> optional(field("label", string)),
      licensePlate: json |> optional(field("license_plate", string))
    };
  let scheduleRelationship = (json) =>
    Json.Decode.(
      switch (int(json)) {
      | 0 => Scheduled
      | 1 => Added
      | 3 => Canceled
      | _ => Unscheduled
      }
    );
  let stopTimeUpdate = (json) =>
    Json.Decode.{
      stopId: json |> optional(field("stop_id", string)),
      arrival: json |> optional(field("arrival", stopTimeEvent)),
      departure: json |> optional(field("departure", stopTimeEvent))
    };
  let tripDescriptor = (json) =>
    Json.Decode.{
      id: json |> optional(field("trip_id", string)),
      routeId: json |> optional(field("route_id", string)),
      directionId: json |> optional(field("direction_id", int)),
      startDate: json |> optional(field("start_date", string)),
      scheduleRelationship: json |> optional(field("schedule_relationship", scheduleRelationship))
    };
  let tripUpdate = (json) =>
    Json.Decode.{
      trip: json |> field("trip", tripDescriptor),
      stopTimeUpdate: json |> optional(field("stop_time_update", list(stopTimeUpdate))),
      timestamp: json |> optional(field("timestamp", int)),
      delay: json |> optional(field("delay", int))
    };
  let vehicleStopStatus = (json) =>
    switch (Json.Decode.int(json)) {
    | 0 => IncomingAt
    | 1 => StoppedAt
    | _ => InTransitTo
    };
  let vehiclePosition = (json) =>
    Json.Decode.{
      trip: json |> optional(field("trip", tripDescriptor)),
      currentStopSequence: json |> optional(field("current_stop_sequence", int)),
      stopId: json |> optional(field("stop_id", string)),
      currentStatus: json |> optional(field("current_status", vehicleStopStatus)),
      timestamp: json |> optional(field("timestamp", int))
    };
  let timeRange = (json) =>
    Json.Decode.{
      startTime: json |> optional(field("start", int)),
      endTime: json |> optional(field("end", int))
    };
  let entitySelector = (json) =>
    Json.Decode.{trip: json |> optional(field("trip", tripDescriptor))};
  let alert = (json) =>
    Json.Decode.{
      informedEntity: json |> field("informed_entity", list(entitySelector)),
      headerText: json |> field("header_text", string)
    };
  let feedEntity = (json) =>
    Json.Decode.{
      id: json |> field("id", string),
      tripUpdate: json |> optional(field("trip_update", tripUpdate)),
      vehicle: json |> optional(field("vehicle", vehiclePosition)),
      alert: json |> optional(field("alert", alert))
    };
  let feedMessage = (json) => Json.Decode.{entity: json |> field("entity", list(feedEntity))};
};

let withApiKey = (apiKey, uri) => uri ++ "&key=" ++ apiKey;

let withFeedId = (feedId, uri) => uri ++ "&feed_id=" ++ feedId;

let fetch_ = (~apiKey, ~feedId) =>
  "http://datamine.mta.info/mta_esi.php?"
  |> withApiKey(apiKey)
  |> withFeedId(feedId)
  |> Bs_fetch.fetch
  |> then_(Bs_fetch.Response.arrayBuffer)
  |> then_((arr) => bufferFrom(arr) |> resolve)
  |> then_((buf) => GTFS.decode(buf) |> resolve)
  |> then_((gtfs) => Decode.feedMessage(gtfs) |> resolve);

let containsUpdateWithStopId = (stopId, stu) =>
  List.fold_left(
    (acc, s: stopTimeUpdate) =>
      ! Option.default(false, acc) ? equals(stopId, s.stopId) : return(true),
    None,
    stu
  );

let findUpdatesByStopId = (stopId, fm) =>
  Lens.over(
    Lenses.entity,
    List.filter(
      (t) =>
        t.tripUpdate
        >>= ((u) => u.stopTimeUpdate)
        >>= containsUpdateWithStopId(stopId)
        |> Option.default(false)
    ),
    fm
  );
