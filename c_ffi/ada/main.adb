with Ada.Text_IO;          use Ada.Text_IO;
with Argon2;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C;         use Interfaces.C;
with Ada.Command_Line;

procedure Main is
    Encoded  : constant char_array :=
       To_C ("$argon2id$v=19$m=16,t=2,p=1$c2FsdC02Nzg$XUb4CH1RppZ2CLAi36TUrw");
    Password : constant char_array := To_C (Ada.Command_Line.Argument (1));
begin
    if Ada.Command_Line.Argument_Count < 1 then
        Put_Line ("Usage: main <encoded_hash>");
        return;
    end if;

    -- I think I'm stripping the null delimiter here
    Put_Line ("Password length is: " & Integer'Image (Password'Length - 1));

    if Argon2.Argon2id_Verify (Encoded, Password, Password'Length - 1) = 0 then
        Put_Line ("Password is correct.");
    else
        Put_Line ("Password is incorrect.");
    end if;
end Main;
