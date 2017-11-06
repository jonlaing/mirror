module Option = {
  let (>>=) = Option.bind;
  let return = Option.Monad.return;
  let equals = (cmp, opt) => opt >>= ((a) => return(a == cmp));
};

module React = {
  let ste = ReasonReact.stringToElement;
};
