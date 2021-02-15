with Ada.Text_IO;
with Ada.Exceptions;
with AdaBaseXClient;
use Ada.Text_IO;
use Ada.Exceptions;
use AdaBaseXClient;

procedure QueryExample is
begin

   if (Connect ("localhost", 1_984) = False) then
      Ada.Text_IO.Put_Line ("Connect failed.");
      return;
   else
      if (Authenticate ("admin", "admin") = False) then
         Ada.Text_IO.Put_Line ("Authenticate failed.");

         Close;
         return;
      end if;
   end if;

   declare
      Response : Query :=
        CreateQuery
          ("declare variable $name external; for $i in 1 to 10 return element { $name } { $i }");
   begin
      Response.Bind ("name", "number", "");
      Ada.Text_IO.Put_Line (Response.Execute);
      Response.Close;
   end;

   Close;

exception
   when Error : BaseXException =>
      Ada.Text_IO.Put ("Exception: ");
      Ada.Text_IO.Put_Line (Exception_Name (Error));
      Ada.Text_IO.Put (Exception_Message (Error));
end QueryExample;
