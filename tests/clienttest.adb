with Ahven;          use Ahven;
with AdaBaseXClient; use AdaBaseXClient;

--
--  Requires BaseX server when testing
--

package body ClientTest is
     procedure Initialize (T : in out Test) is
     begin
          Set_Name (T, "My tests");

          T.Add_Test_Routine
              (Routine => Unknown_Fail'Access, Name => "Unknown_Fail");

          T.Add_Test_Routine
              (Routine => Port_Fail'Access, Name => "Port_Fail");

          T.Add_Test_Routine
              (Routine => Connect_Auth'Access, Name => "Connect_Auth");

          T.Add_Test_Routine
              (Routine => Connect_Pass'Access, Name => "Connect_Pass");

          T.Add_Test_Routine
              (Routine => Execute_Test'Access, Name => "Execute_Test");

          T.Add_Test_Routine
              (Routine => Execute_Fail'Access, Name => "Execute_Fail");

          T.Add_Test_Routine
              (Routine => Create_Drop'Access, Name => "Create_Drop");

          T.Add_Test_Routine (Routine => Add_Test'Access, Name => "Add_Test");

          T.Add_Test_Routine
              (Routine => Replace_Test'Access, Name => "Replace_Test");

          T.Add_Test_Routine
              (Routine => Query_Test'Access, Name => "Query_Test");

          T.Add_Test_Routine
              (Routine => Query_Bind'Access, Name => "Query_Bind");

     end Initialize;

     procedure Unknown_Fail is

          result : Boolean;

     begin
          result := Connect ("unknown", 1_984);
          Fail (Message => "Exception expected");

     exception
          when Error : BaseXException =>
               null;

     end Unknown_Fail;

     procedure Port_Fail is

          result : Boolean;

     begin
          result := Connect ("localhost", 1_985);
          Fail (Message => "Exception expected");

     exception
          when Error : BaseXException =>
               null;

     end Port_Fail;

     procedure Connect_Auth is

          result : Boolean;

     begin
          result := Connect ("localhost", 1_984);
          Assert (Condition => result = True, Message => "Connect test");

          result := Authenticate ("unknown", "test");
          Assert (Condition => result = False, Message => "Auth test");

          Close;

     end Connect_Auth;

     procedure Connect_Pass is

          result : Boolean;

     begin
          result := Connect ("localhost", 1_984);
          Assert (Condition => result = True, Message => "Connect test");

          result := Authenticate ("admin", "admin");
          Assert (Condition => result = True, Message => "Auth test");

          Close;

     end Connect_Pass;

     procedure Execute_Test is

          result : Boolean;

     begin
          result := Connect ("localhost", 1_984);
          Assert (Condition => result = True, Message => "Connect test");

          result := Authenticate ("admin", "admin");
          Assert (Condition => result = True, Message => "Auth test");

          declare
               Response : String := Execute ("xquery 1+1");
          begin
               Assert (Condition => Response = "2", Message => "Execute test");
          end;

          Close;

     end Execute_Test;

     procedure Execute_Fail is

          result : Boolean;

     begin
          result := Connect ("localhost", 1_984);
          Assert (Condition => result = True, Message => "Connect test");

          result := Authenticate ("admin", "admin");
          Assert (Condition => result = True, Message => "Auth test");

          declare
               Response : String := Execute ("xquery unknown");
          begin
               Fail (Message => "Exception expected");
          end;

     exception
          when Error : BaseXException =>
               Close;

     end Execute_Fail;

     procedure Create_Drop is

          result : Boolean;

     begin
          result := Connect ("localhost", 1_984);
          Assert (Condition => result = True, Message => "Connect test");

          result := Authenticate ("admin", "admin");
          Assert (Condition => result = True, Message => "Auth test");

          declare
               Response : String := Create ("database", "<x>Hello World!</x>");
          begin
               null;
          end;

          declare
               Response : String := Execute ("xquery /");
          begin
               Assert
                   (Condition => Response = "<x>Hello World!</x>",
                    Message   => "Query database");
          end;

          declare
               Response : String := Execute ("drop db database");
          begin
               null;
          end;

          Close;

     end Create_Drop;

     procedure Add_Test is

          result : Boolean;

     begin
          result := Connect ("localhost", 1_984);
          Assert (Condition => result = True, Message => "Connect test");

          result := Authenticate ("admin", "admin");
          Assert (Condition => result = True, Message => "Auth test");

          declare
               Response : String := Create ("database", "");
          begin
               null;
          end;

          declare
               Response : String := Add ("Ada.xml", "<x>Hello Ada!</x>");
          begin
               null;
          end;

          declare
               Response : String := Execute ("xquery /");
          begin
               Assert
                   (Condition => Response = "<x>Hello Ada!</x>",
                    Message   => "Query database");
          end;

          declare
               Response : String := Execute ("drop db database");
          begin
               null;
          end;

          Close;

     end Add_Test;

     procedure Replace_Test is

          result : Boolean;

     begin
          result := Connect ("localhost", 1_984);
          Assert (Condition => result = True, Message => "Connect test");

          result := Authenticate ("admin", "admin");
          Assert (Condition => result = True, Message => "Auth test");

          declare
               Response : String := Create ("database", "");
          begin
               null;
          end;

          declare
               Response : String := Add ("Ada.xml", "<x>Hello Ada!</x>");
          begin
               null;
          end;

          declare
               Response : String :=
                   Replace ("Ada.xml", "<x>Ada is awesome!</x>");
          begin
               null;
          end;

          declare
               Response : String := Execute ("xquery /");
          begin
               Assert
                   (Condition => Response = "<x>Ada is awesome!</x>",
                    Message   => "Query database");
          end;

          declare
               Response : String := Execute ("drop db database");
          begin
               null;
          end;

          Close;

     end Replace_Test;

     procedure Query_Test is

          result : Boolean;

     begin
          result := Connect ("localhost", 1_984);
          Assert (Condition => result = True, Message => "Connect test");

          result := Authenticate ("admin", "admin");
          Assert (Condition => result = True, Message => "Auth test");

          declare
               Response : Query := CreateQuery ("1+1");
               V        : String_Vectors.Vector;
          begin
               V := Response.Results;
               Assert (Condition => V (0) = "2", Message => "Query test");

               Response.Close;
          end;

          Close;

     end Query_Test;

     procedure Query_Bind is

          result : Boolean;

     begin
          result := Connect ("localhost", 1_984);
          Assert (Condition => result = True, Message => "Connect test");

          result := Authenticate ("admin", "admin");
          Assert (Condition => result = True, Message => "Auth test");

          declare
               Response : Query :=
                   CreateQuery
                       ("declare variable $name external; for $i in 1 to 1 return element { $name } { $i }");
          begin
               Response.Bind ("name", "number", "");
               declare
                    Res : String := Response.Execute;
               begin
                    Assert
                        (Condition => Res = "<number>1</number>",
                         Message   => "Query bind");
               end;

               Response.Close;
          end;

          Close;

     end Query_Bind;

end ClientTest;
