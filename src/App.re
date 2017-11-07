open ReactNative;

open Components;

let component = ReasonReact.statelessComponent("App");

let make = (~weatherApiKey, _children) => {
  ...component,
  render: (_self) => <View> <Weather apiKey=weatherApiKey /> </View>
};

let default =
  ReasonReact.wrapReasonForJs(
    ~component,
    (jsProps) => make(~weatherApiKey=jsProps##weatherApiKey, [||])
  );
