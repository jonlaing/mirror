module FeedMessage = {
  type t;
  [@bs.send] external decode : (t, 'something) => 'somethingElse = "";
};

[@bs.module "gtfs-realtime-bindings"] external feedMessage : FeedMessage.t = "FeedMessage";

let decode = (res) => FeedMessage.decode(feedMessage, res);
