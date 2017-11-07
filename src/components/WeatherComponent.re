open ReactNative;

open Js.Promise;

open Utils;

let precToStr = (p) =>
  switch p {
  | Weather.Rain(n) => "Rain: " ++ Js.Int.toString(n)
  | Weather.Snow(n) => "Snow: " ++ Js.Int.toString(n)
  | Weather.RainSnow(a, b) => "Rain: " ++ Js.Int.toString(a) ++ " Snow: " ++ Js.Int.toString(b)
  | Weather.NoPrecipitation => ""
  };

type action =
  | FetchCurrentSuccess(Weather.t(Temp.kelvin))
  | FetchNextSuccess(list(Weather.t(Temp.kelvin)))
  | FetchFail(Js.Promise.error);

type status =
  | Init
  | Success
  | Error(Js.Promise.error);

type state = {
  current: Weather.t(Temp.farenheit),
  next: list(Weather.t(Temp.farenheit)),
  status,
  currentTimerId: ref(option(Js.Global.intervalId)),
  nextTimerId: ref(option(Js.Global.intervalId))
};

let zip = "11221";

let currentSeconds = () => Js.Date.now() /. 1000.0;

let component = ReasonReact.reducerComponent("Weather");

let fetchCurrent = (apiKey) =>
  Forecast.fetchCurrentByZip(~apiKey, ~zip) |> then_((res) => Weather.fromCurrent(res) |> resolve);

let fetchNext = (apiKey) =>
  Forecast.fetchNextByZip(~apiKey, ~zip)
  |> then_((res) => Weather.fromNext(res) |> resolve)
  |> then_(
       (ws) =>
         ws
         |> BatList.filter((w) => w.Weather.timestamp >= currentSeconds())
         |> BatList.take(4)
         |> resolve
     );

let make = (~apiKey, _children) => {
  ...component,
  initialState: () => {
    current: Weather.make() |> Weather.kelvinToFarenheit,
    next: BatList.make(0, Weather.make() |> Weather.kelvinToFarenheit),
    status: Init,
    currentTimerId: ref(None),
    nextTimerId: ref(None)
  },
  didMount: (self) => {
    let current = () =>
      fetchCurrent(apiKey)
      |> then_((w) => self.reduce((_) => FetchCurrentSuccess(w), ()) |> resolve)
      |> catch((err) => self.reduce((_) => FetchFail(err), ()) |> resolve);
    let next = () =>
      fetchNext(apiKey)
      |> then_((ws) => self.reduce((_) => FetchNextSuccess(ws), ()) |> resolve)
      |> catch((err) => self.reduce((_) => FetchFail(err), ()) |> resolve);
    let _ = current();
    let _ = next();
    self.state.currentTimerId :=
      Some(
        Js.Global.setInterval(
          () => {
            let _ = current();
            ()
          },
          10000
        )
      );
    self.state.nextTimerId :=
      Some(
        Js.Global.setInterval(
          () => {
            let _ = next();
            ()
          },
          1000 * 60 * 5
        )
      );
    ReasonReact.NoUpdate
  },
  reducer: (action, state) =>
    switch action {
    | FetchCurrentSuccess(w) =>
      ReasonReact.Update({...state, status: Success, current: Weather.kelvinToFarenheit(w)})
    | FetchNextSuccess(ws) =>
      ReasonReact.Update({
        ...state,
        status: Success,
        next: List.map(Weather.kelvinToFarenheit, ws)
      })
    | FetchFail(error) =>
      Js.log(error);
      ReasonReact.Update({...state, status: Error(error)})
    },
  render: ({state}) =>
    switch state.status {
    | Init => <View> <Text> (ste("Loading...")) </Text> </View>
    | Error(_) => <View> <Text> (ste("error")) </Text> </View>
    | _ =>
      <View>
        <Text> (state.current.temp |> Temp.degrees |> ste) </Text>
        <Text> (state.current.title |> ste) </Text>
        <Text> (state.current.precipitation |> precToStr |> ste) </Text>
        (
          state.next
          |> BatList.map(
               (w) =>
                 <View key=(w.Weather.timestamp |> Js.Float.toString)>
                   <Text>
                     MomentRe.(
                       momentWithUnix(w.Weather.timestamp |> Js.Math.floor_int)
                       |> Moment.fromNow(~withoutSuffix=Some(false))
                       |> ste
                     )
                   </Text>
                   <Text> (ste(w.Weather.title)) </Text>
                 </View>
             )
          |> Array.of_list
          |> ReasonReact.arrayToElement
        )
      </View>
    }
};
