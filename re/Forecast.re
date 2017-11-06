type weather = {
  id: int,
  title: string,
  description: string,
  icon: string
};

type additional = {
  temp: Temp.t(Temp.kelvin),
  pressure: float,
  humidity: int,
  tempMin: Temp.t(Temp.kelvin),
  tempMax: Temp.t(Temp.kelvin),
  visibility: option(int),
  name: option(string)
};

type current = {
  timestamp: float,
  basic: list(weather),
  clouds: option(int),
  rain: option(int),
  snow: option(int),
  additional
};

type next = {list: list(current)};

module Decode = {
  let hr3 = (json) => Json.Decode.(json |> field("3hr", int));
  let all = (json) => Json.Decode.(json |> field("all", int));
  let weather = (json) =>
    Json.Decode.{
      id: json |> field("id", int),
      title: json |> field("main", string),
      description: json |> field("description", string),
      icon: json |> field("icon", string)
    };
  let additional = (json) => {
    temp: json |> Json.Decode.field("temp", Json.Decode.float) |> Temp.ofKelvin,
    pressure: Json.Decode.(json |> field("pressure", Json.Decode.float)),
    humidity: Json.Decode.(json |> field("humidity", int)),
    tempMin: json |> Json.Decode.field("temp_min", Json.Decode.float) |> Temp.ofKelvin,
    tempMax: json |> Json.Decode.field("temp_max", Json.Decode.float) |> Temp.ofKelvin,
    visibility: Json.Decode.(json |> optional(field("visibility", int))),
    name: Json.Decode.(json |> optional(field("name", string)))
  };
  let current = (json) =>
    Json.Decode.{
      timestamp: json |> field("dt", Json.Decode.float),
      basic: json |> field("weather", list(weather)),
      clouds: json |> optional(field("clouds", all)),
      rain: json |> optional(field("rain", hr3)),
      snow: json |> optional(field("snow", hr3)),
      additional: json |> field("main", additional)
    };
  let next = (json) => Json.Decode.{list: json |> field("list", list(current))};
};

let withAPIKey = (key, uri) => uri ++ "&APPID=" ++ key;

let withLatLong = (lat, long, uri) =>
  uri ++ "&lat=" ++ Js.Float.toString(lat) ++ "&long=" ++ Js.Float.toString(long);

let withZip = (zip, uri) => uri ++ "&zip=" ++ zip ++ ",us";

let fetch_ = (decode, apiKey, uri) =>
  Js.Promise.(
    withAPIKey(apiKey, uri)
    |> Bs_fetch.fetch
    |> then_(Bs_fetch.Response.json)
    |> then_((json) => decode(json) |> resolve)
  );

let fetchCurrent = fetch_(Decode.current);

let fetchNext = fetch_(Decode.next);

let fetchCurrentByCoords = (~apiKey, ~latitude, ~longitude) =>
  "https://api.openweathermap.org/data/2.5/weather?"
  |> withLatLong(latitude, longitude)
  |> fetchCurrent(apiKey);

let fetchCurrentByZip = (~apiKey, ~zip) =>
  "https://api.openweathermap.org/data/2.5/weather?" |> withZip(zip) |> fetchCurrent(apiKey);

let fetchNextByCoords = (~apiKey, ~latitude, ~longitude) =>
  "https://api.openweathermap.org/data/2.5/forecast?"
  |> withLatLong(latitude, longitude)
  |> fetchNext(apiKey);

let fetchNextByZip = (~apiKey, ~zip) =>
  "https://api.openweathermap.org/data/2.5/forecast?" |> withZip(zip) |> fetchNext(apiKey);
