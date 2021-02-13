----------------------------------------------------------------------------
--
--  Ada client for BaseX
--
----------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;
with GNAT.Sockets;

package AdaBaseXClient is

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Natural, String);
   use String_Vectors;

   --
   --  BaseX exception
   --
   BaseXException : exception;

   --
   --  For Query Command Protocol
   --
   type Query is tagged private;

   --
   --  Inserts a document in the database at the specified path
   --
   function Add (Path : String; Input : String) return String;

   --
   --  Authenticate for this session
   --
   function Authenticate (Username : String; Password : String) return Boolean;

   --
   --  Bind procedure for Query class
   --
   procedure Bind
     (Self : Query; Name : String; Value : String; Stype : String);

   --
   --  Close a connection to the host
   --
   procedure Close;

   --
   --  Close procedure for Query class
   --
   procedure Close (Self : out Query);

   --
   --  Open a connection to the host
   --
   function Connect (Server : String; Port : Natural) return Boolean;

   --
   --  Create a new database, inserts initial content
   --
   function Create (Name : String; Input : String) return String;

   --
   --  Create a new Query instance
   --
   function CreateQuery (Qstring : String) return Query;

   --
   --  Execute BaseX command
   --
   function Execute (command : String) return String;

   --
   --  Execute function for Query class
   --
   function Execute (Self : Query) return String;

   --
   --  Return process information
   --
   function Info return String;

   --
   --  Initialize procedure for Query class
   --
   procedure Initialize (Self : out Query; MyId : String);

   --
   --  Replaces content at the specified path by the given document
   --
   function Replace (Path : String; Input : String) return String;

   --
   --  Results function for Query class
   --  Returns all resulting items as strings
   --
   function Results (Self : Query) return String_Vectors.Vector;

   --
   --  Stores a binary resource in the opened database
   --
   function Store (Path : String; Input : String) return String;

private

   --
   --  Socket variable
   --
   Socket  : GNAT.Sockets.Socket_Type;
   Channel : GNAT.Sockets.Stream_Access;

   type Query is tagged record
      Id : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --
   --  Read data from server
   --
   function Read return String;

   --
   --  Read string from server
   --
   function ReadString return String;

   --
   --  Send data to server
   --
   procedure Send (Command : String);

   --
   --  Send Command to server
   --
   procedure SendCmd (Code : Natural; Arg : String; Input : String);

   --
   --  Read status single byte from socket.
   --  Server replies with \00 (success) or \01 (error).
   --
   function Status return Boolean;

end AdaBaseXClient;
