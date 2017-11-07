open ReactNative;

open Js.Promise;

type status =
  | Init
  | Success
  | Error(Js.Promise.error);

type state = {
  feedMessage: option(SubwayTimes.feedMessage),
  status
};

type action =
  | FetchSuccess(SubwayTimes.feedMessage)
  | FetchFail(Js.Promise.error);

let component = ReasonReact.reducerComponent("SubwayTimes");

/* let fetchTimes = (reduce, apiKey, feedId, stopId, ()) =>
     SubwayTimes.fetch_(~apiKey, ~feedId)
     |> then_((fm) => SubwayTimes.findUpdatesByStopId(stopId, fm) |> BatList.take(5) |> resolve);
   |> then_((fm) => reduce((_) => FetchSuccess(fm))) */
let make = (~apiKey, ~stopId, _children) => {
  ...component,
  initialState: () => {feedMessage: None, status: Init},
  reducer: (action, state) =>
    switch action {
    | FetchSuccess(msg) => ReasonReact.Update({feedMessage: Some(msg), status: Success})
    | FetchFail(err) => ReasonReact.Update({...state, status: Error(err)})
    },
  render: ({state}) => <View />
};
