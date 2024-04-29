open Ctypes
module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  let argon2id_verify =
    F.foreign "argon2id_verify" F.(string @-> string @-> size_t @-> returning int)
  ;;
end
