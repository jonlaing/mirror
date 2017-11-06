type t('a) = {temp: float};

type kelvin;

type celcius;

type farenheit;

let ofKelvin: float => t(kelvin) = (f) => {temp: f};

let ofCelcius: float => t(celcius) = (f) => {temp: f};

let ofFarenheit: float => t(farenheit) = (f) => {temp: f};

let kelvinToCelcius: t(kelvin) => t(celcius) = ({temp}) => {temp: temp -. 275.15};

let celciusToKelvin: t(celcius) => t(kelvin) = ({temp}) => {temp: temp +. 275.15};

let celciusToFarenheit: t(celcius) => t(farenheit) =
  ({temp}) => {temp: temp *. 9.0 /. 5.0 +. 32.0};

let farenheitToCelcius: t(farenheit) => t(celcius) =
  ({temp}) => {temp: (temp -. 32.0) *. 5.0 /. 9.0};

let kelvinToFarenheit = (k) => k |> kelvinToCelcius |> celciusToFarenheit;

let farenheitToKelvin = (f) => f |> farenheitToCelcius |> celciusToKelvin;

let toFloat = ({temp}) => temp;

let degrees = (w) => (w |> toFloat |> Js.Float.toFixed) ++ "\176";

let (+): (t('a), t('a)) => t('a) = ({temp: t0}, {temp: t1}) => {temp: t0 +. t1};

let (-): (t('a), t('a)) => t('a) = ({temp: t0}, {temp: t1}) => {temp: t0 -. t1};
