type ('s, 'a) t = 's -> 'a * 's

let return x s = x, s

let bind m f s =
  let x, s' = m s in
  f x s'
;;

let get s = s, s
let modify f s = (), f s
