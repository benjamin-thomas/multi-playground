with System;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C;         use Interfaces.C;

package Argon2 is
    function Argon2id_Verify
       (Encoded : char_array; Pwd : char_array; Pwdlen : size_t) return int;

    pragma Import (C, Argon2id_Verify, "argon2id_verify");
end Argon2;
