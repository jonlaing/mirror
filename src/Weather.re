type precipitation =
  | Rain(int)
  | Snow(int)
  | RainSnow(int, int)
  | NoPrecipitation;

type t('a) = {
  timestamp: float,
  temp: Temp.t('a),
  tempMax: Temp.t('a),
  tempMin: Temp.t('a),
  code: int,
  title: string,
  precipitation
};

let make = () => {
  timestamp: Js.Date.now(),
  temp: Temp.ofKelvin(0.0),
  tempMax: Temp.ofKelvin(0.0),
  tempMin: Temp.ofKelvin(0.0),
  code: 0,
  title: "Loading...",
  precipitation: NoPrecipitation
};

let kelvinToCelcius: t(Temp.kelvin) => t(Temp.celcius) =
  (w) => {
    ...w,
    temp: Temp.kelvinToCelcius(w.temp),
    tempMax: Temp.kelvinToCelcius(w.tempMax),
    tempMin: Temp.kelvinToCelcius(w.tempMin)
  };

let celciusToKelvin: t(Temp.celcius) => t(Temp.kelvin) =
  (w) => {
    ...w,
    temp: Temp.celciusToKelvin(w.temp),
    tempMax: Temp.celciusToKelvin(w.tempMax),
    tempMin: Temp.celciusToKelvin(w.tempMin)
  };

let celciusToFarenheit: t(Temp.celcius) => t(Temp.farenheit) =
  (w) => {
    ...w,
    temp: Temp.celciusToFarenheit(w.temp),
    tempMax: Temp.celciusToFarenheit(w.tempMax),
    tempMin: Temp.celciusToFarenheit(w.tempMin)
  };

let farenheitToCelcius: t(Temp.farenheit) => t(Temp.celcius) =
  (w) => {
    ...w,
    temp: Temp.farenheitToCelcius(w.temp),
    tempMax: Temp.farenheitToCelcius(w.tempMax),
    tempMin: Temp.farenheitToCelcius(w.tempMin)
  };

let farenheitToKelvin: t(Temp.farenheit) => t(Temp.kelvin) =
  (w) => {
    ...w,
    temp: Temp.farenheitToKelvin(w.temp),
    tempMax: Temp.farenheitToKelvin(w.tempMax),
    tempMin: Temp.farenheitToKelvin(w.tempMin)
  };

let kelvinToFarenheit: t(Temp.kelvin) => t(Temp.farenheit) =
  (w) => {
    ...w,
    temp: Temp.kelvinToFarenheit(w.temp),
    tempMax: Temp.kelvinToFarenheit(w.tempMax),
    tempMin: Temp.kelvinToFarenheit(w.tempMin)
  };

let fromCurrent = (forecast) => {
  let prec = (f: Forecast.current) =>
    switch (f.rain, f.snow) {
    | (Some(a), Some(b)) => RainSnow(a, b)
    | (Some(a), _) => Rain(a)
    | (_, Some(b)) => Snow(b)
    | (_, _) => NoPrecipitation
    };
  {
    timestamp: forecast.Forecast.timestamp,
    temp: forecast.Forecast.additional.Forecast.temp,
    tempMax: forecast.additional.Forecast.tempMax,
    tempMin: forecast.additional.Forecast.tempMin,
    code: List.hd(forecast.basic).Forecast.id,
    title: List.hd(forecast.basic).Forecast.title,
    precipitation: prec(forecast)
  }
};

let fromNext = (forecast: Forecast.next) => forecast.list |> List.map(fromCurrent);
