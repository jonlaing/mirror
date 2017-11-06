open ReactNative;

let component = ReasonReact.statelessComponent("App");

let make = (~weatherApiKey, _children) => {
  ...component,
  render: (_self) => <View> <WeatherComponent apiKey=weatherApiKey /> </View>
};

let default =
  ReasonReact.wrapReasonForJs(
    ~component,
    (jsProps) => make(~weatherApiKey=jsProps##weatherApiKey, [||])
  );
