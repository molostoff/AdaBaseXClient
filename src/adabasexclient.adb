----------------------------------------------------------------------------
--
--  Ada client for BaseX
--
----------------------------------------------------------------------------

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Exceptions;
with Ada.Streams;
with GNAT.MD5;

package body AdaBaseXClient is

   use Ada.Strings.Unbounded;
   use Ada.Exceptions;
   use GNAT.Sockets;

   --
   --  Inserts a document in the database at the specified path
   --
   function Add (Path : String; Input : String) return String is
      Response      : Ada.Strings.Unbounded.Unbounded_String;
      result_status : Boolean := True;
   begin
      SendCmd (9, Path, Input);

      Response := To_Unbounded_String (ReadString);
      Response :=
        To_Unbounded_String (Slice (Response, 1, Length (Response) - 1));

      result_status := Status;
      if result_status = False then
         raise BaseXException with To_String (Response);
      end if;

      return To_String (Response);

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";

   end Add;

   --
   --  Authenticate for this session
   --
   function Authenticate (Username : String; Password : String) return Boolean
   is
      Nonce  : Ada.Strings.Unbounded.Unbounded_String;
      Realm  : Ada.Strings.Unbounded.Unbounded_String;
      Output : Ada.Strings.Unbounded.Unbounded_String;
      Ret    : Ada.Strings.Unbounded.Unbounded_String;
   begin

      Ret := To_Unbounded_String (ReadString);

      declare
         Input  : constant String := To_String (Ret);
         Start  : Positive        := Input'First;
         Finish : Natural         := 0;
      begin
         Ret := To_Unbounded_String (Username);
         Send (To_String (Ret));

         Ada.Strings.Fixed.Find_Token
           (Input, Ada.Strings.Maps.To_Set (':'), Start, Ada.Strings.Outside,
            Start, Finish);

         if Finish = Input'Last then
            Nonce := To_Unbounded_String (Input (Start .. Input'Last - 1));

            Output := To_Unbounded_String (Password);
            Ret    :=
              To_Unbounded_String
                (GNAT.MD5.Digest
                   (GNAT.MD5.Digest (To_String (Output)) & To_String (Nonce)));
         else
            Realm := To_Unbounded_String (Input (Start .. Finish));
            Nonce :=
              To_Unbounded_String (Input (Finish + 2 .. Input'Last - 1));

            Output :=
              To_Unbounded_String
                (Username & ":" & To_String (Realm) & ":" & Password);
            Ret :=
              To_Unbounded_String
                (GNAT.MD5.Digest
                   (GNAT.MD5.Digest (To_String (Output)) & To_String (Nonce)));
         end if;
      end;
      Send (To_String (Ret));

      if Status then
         return True;
      end if;

      return False;

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";

   end Authenticate;

   --
   --  Bind procedure for Query class
   --
   procedure Bind
     (Self : Query; Name : String; Value : String; Stype : String)
   is
      Response      : Ada.Strings.Unbounded.Unbounded_String;
      result_status : Boolean := True;
   begin

      Send
        (Character'Val (3) & To_String (Self.Id) & Character'Val (0) & Name &
         Character'Val (0) & Value & Character'Val (0) & Stype);

      Response := To_Unbounded_String (ReadString);
      Response :=
        To_Unbounded_String (Slice (Response, 1, Length (Response) - 1));

      result_status := Status;
      if result_status = False then
         raise BaseXException with "Bind query failed";
      end if;

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";
   end Bind;

   --
   --  Close a connection to the host
   --
   procedure Close is
   begin
      Send ("exit");

      Free (Channel);
      Close_Socket (Socket);

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";
   end Close;

   --
   --  Close procedure for Query class
   --
   procedure Close (Self : out Query) is
      Response      : Ada.Strings.Unbounded.Unbounded_String;
      result_status : Boolean := True;
   begin

      Send (Character'Val (2) & To_String (Self.Id));

      Response := To_Unbounded_String (ReadString);
      Response :=
        To_Unbounded_String (Slice (Response, 1, Length (Response) - 1));

      result_status := Status;
      if result_status = False then
         raise BaseXException with "Close query failed";
      end if;

      --  Delete Query Id
      Delete (Self.Id, 1, Length (Self.Id));

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";
   end Close;

   --
   --  Open a connection to the host
   --
   function Connect (Server : String; Port : Natural) return Boolean is
      Address : GNAT.Sockets.Sock_Addr_Type;
   begin

      Address.Addr :=
        GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name (Server), 1);
      Address.Port := Port_Type (Port);
      GNAT.Sockets.Create_Socket (AdaBaseXClient.Socket);
      GNAT.Sockets.Connect_Socket (AdaBaseXClient.Socket, Address);

      AdaBaseXClient.Channel := Stream (AdaBaseXClient.Socket);

      return True;

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";

   end Connect;

   --
   --  Create a new database, inserts initial content
   --
   function Create (Name : String; Input : String) return String is
      Response      : Ada.Strings.Unbounded.Unbounded_String;
      result_status : Boolean := True;
   begin
      SendCmd (8, Name, Input);

      Response := To_Unbounded_String (ReadString);

      result_status := Status;
      if result_status = False then
         raise BaseXException with To_String (Response);
      end if;

      return To_String (Response);

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";

   end Create;

   --
   --  Create a new Query instance
   --
   function CreateQuery (Qstring : String) return Query is
      MyQuery       : Query;
      Response      : Ada.Strings.Unbounded.Unbounded_String;
      result_status : Boolean := True;
   begin

      Send (Character'Val (0) & Qstring);

      Response := To_Unbounded_String (ReadString);
      Response :=
        To_Unbounded_String (Slice (Response, 1, Length (Response) - 1));

      result_status := Status;
      if result_status = False then
         raise BaseXException with "Create Query failed";
      end if;

      --  Assign Id to Query class
      MyQuery.Initialize (To_String (Response));

      return MyQuery;

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";

   end CreateQuery;

   --
   --  Execute BaseX command
   --
   function Execute (command : String) return String is
      Response      : Ada.Strings.Unbounded.Unbounded_String;
      Info          : Ada.Strings.Unbounded.Unbounded_String;
      result_status : Boolean := True;
   begin
      Send (command);

      Response := To_Unbounded_String (ReadString);
      Response :=
        To_Unbounded_String (Slice (Response, 1, Length (Response) - 1));
      Info := To_Unbounded_String (ReadString);
      Info := To_Unbounded_String (Slice (Info, 1, Length (Info) - 1));

      result_status := Status;
      if result_status = False then
         raise BaseXException with To_String (Info);
      end if;

      return To_String (Response);

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";

   end Execute;

   --
   --  Execute function for Query class
   --
   function Execute (Self : Query) return String is
      Response      : Ada.Strings.Unbounded.Unbounded_String;
      result_status : Boolean := True;
   begin

      Send (Character'Val (5) & To_String (Self.Id));

      Response := To_Unbounded_String (ReadString);
      Response :=
        To_Unbounded_String (Slice (Response, 1, Length (Response) - 1));

      result_status := Status;
      if result_status = False then
         raise BaseXException with "Execute query failed";
      end if;

      return (To_String (Response));

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";
   end Execute;

   --
   --  Return process information
   --
   function Info return String is
      Response      : Ada.Strings.Unbounded.Unbounded_String;
      result_status : Boolean := True;
   begin
      Send ("INFO");

      Response := To_Unbounded_String (ReadString);

      result_status := Status;
      if result_status = False then
         raise BaseXException with "Return process information failed";
      end if;

      return To_String (Response);

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";

   end Info;

   --
   --  Initialize procedure for Query class
   --
   procedure Initialize (Self : out Query; MyId : String) is
   begin

      Self.Id := To_Unbounded_String (MyId);

   end Initialize;

   --
   --  Read data from server
   --
   function Read return String is
      Data : Ada.Streams.Stream_Element_Array (1 .. 1_024);
      Size : Ada.Streams.Stream_Element_Offset;
      Res  : Ada.Strings.Unbounded.Unbounded_String;
   begin

      GNAT.Sockets.Receive_Socket (Socket, Data, Size);
      for i in 1 .. Size loop
         Res := Res & Character'Val (Data (i));
      end loop;

      return To_String (Res);

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);

   end Read;

   --
   --  Read string from server
   --
   function ReadString return String is
      Data : Ada.Streams.Stream_Element_Array (1 .. 1);
      Size : Ada.Streams.Stream_Element_Offset;
      Res  : Ada.Strings.Unbounded.Unbounded_String;
      Chr  : Character;
   begin

      loop
         GNAT.Sockets.Receive_Socket (Socket, Data, Size);
         Res := Res & Character'Val (Data (1));

         Chr := Character'Val (Data (1));
         if Chr = Character'Val (0) then
            exit;
         end if;
      end loop;

      return To_String (Res);

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);

   end ReadString;

   --
   --  Replaces content at the specified path by the given document
   --
   function Replace (Path : String; Input : String) return String is
      Response      : Ada.Strings.Unbounded.Unbounded_String;
      result_status : Boolean := True;
   begin
      SendCmd (12, Path, Input);

      Response := To_Unbounded_String (ReadString);
      Response :=
        To_Unbounded_String (Slice (Response, 1, Length (Response) - 1));

      result_status := Status;
      if result_status = False then
         raise BaseXException with To_String (Response);
      end if;

      return To_String (Response);

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";

   end Replace;

   --
   --  Results function for Query class
   --  Returns all resulting items as strings
   --
   function Results (Self : Query) return String_Vectors.Vector is
      Response      : Ada.Strings.Unbounded.Unbounded_String;
      result_status : Boolean := True;
      VString       : String_Vectors.Vector;
   begin

      Send (Character'Val (4) & To_String (Self.Id));

      while Status = False loop
         Response := To_Unbounded_String (ReadString);
         Response :=
           To_Unbounded_String (Slice (Response, 1, Length (Response) - 1));

         VString.Append (To_String (Response));
      end loop;

      result_status := Status;
      if result_status = False then
         Response := To_Unbounded_String (ReadString);
         Response :=
           To_Unbounded_String (Slice (Response, 1, Length (Response) - 1));

         raise BaseXException with To_String (Response);
      end if;

      return VString;

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";
   end Results;

   --
   --  Send data to server
   --
   procedure Send (Command : String) is
   begin

      String'Write (AdaBaseXClient.Channel, Command & Character'Val (0));

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);

   end Send;

   --
   --  Send Command to server
   --
   procedure SendCmd (Code : Natural; Arg : String; Input : String) is
   begin

      String'Write
        (AdaBaseXClient.Channel,
         Character'Val (Code) & Arg & Character'Val (0) & Input &
         Character'Val (0));

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);

   end SendCmd;

   --
   --  Read status single byte from socket.
   --  Server replies with \00 (success) or \01 (error).
   --
   function Status return Boolean is
      Data : Ada.Streams.Stream_Element_Array (1 .. 1);
      Size : Ada.Streams.Stream_Element_Offset;
      Res  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      GNAT.Sockets.Receive_Socket (Socket, Data, Size);
      Res := Res & Character'Val (Data (1));

      if Element (Res, 1) = Character'Val (0) then
         return True;
      end if;

      return False;

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);

   end Status;

   --
   --  Stores a binary resource in the opened database
   --
   function Store (Path : String; Input : String) return String is
      Response      : Ada.Strings.Unbounded.Unbounded_String;
      result_status : Boolean := True;
   begin
      SendCmd (13, Path, Input);

      Response := To_Unbounded_String (ReadString);
      Response :=
        To_Unbounded_String (Slice (Response, 1, Length (Response) - 1));

      result_status := Status;
      if result_status = False then
         raise BaseXException with To_String (Response);
      end if;

      return To_String (Response);

   exception
      when E : Socket_Error =>
         raise BaseXException with Exception_Message (E);
      when E : BaseXException =>
         raise BaseXException with Exception_Message (E);
      when others =>
         raise BaseXException with "Unexpected exception";

   end Store;

begin

   null;

end AdaBaseXClient;
