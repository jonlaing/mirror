open ReactNative;

let component = ReasonReact.statelessComponent("App");

let make = (~weatherApiKey, _children) => {
  ...component,
  /* TODO: hardcoding zip for now, will make it configurable */
  render: (_self) => <View> <WeatherComponent apiKey=weatherApiKey zip="11221" /> </View>
};

let default =
  ReasonReact.wrapReasonForJs(
    ~component,
    (jsProps) => make(~weatherApiKey=jsProps##weatherApiKey, [||])
  );
