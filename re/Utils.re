module Option = {
  open Option.Monad;
  let (>>=) = bind;
  let return = return;
  let equals = (cmp, opt) => opt >>= ((a) => return(a == cmp));
};

let ste = ReasonReact.stringToElement;
