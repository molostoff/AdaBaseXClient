with Ahven.Framework;

package ClientTest is
   type Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Test);

   procedure Unknown_Fail;
   procedure Port_Fail;
   procedure Connect_Auth;
   procedure Connect_Pass;
   procedure Execute_Test;
   procedure Execute_Fail;
   procedure Create_Drop;
   procedure Add_Test;
   procedure Replace_Test;
   procedure Query_Test;
   procedure Query_Bind;
end ClientTest;
