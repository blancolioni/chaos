with Ada.Characters.Handling;

with Chaos.Logging;

package body Chaos.Resources is

   -----------
   -- Close --
   -----------

   procedure Close
     (Resource : in out Chaos_Resource'Class)
   is
   begin
      Close (Resource.File);
   end Close;

   ---------------------
   -- End_Of_Resource --
   ---------------------

   function End_Of_Resource
     (Resource : Chaos_Resource'Class)
      return Boolean
   is
   begin
      return WL.Binary_IO.Current_Offset (Resource.File)
        - Resource.Start
        >= Resource.Length;
   end End_Of_Resource;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Res_Ref  : out Resource_Reference)
   is
      use Ada.Characters.Handling;
   begin
      Read (Resource.File, Res_Ref'Size, Res_Ref'Address);
      for I in Res_Ref'Range loop
         if Is_Lower (Res_Ref (I)) then
            Res_Ref (I) := To_Upper (Res_Ref (I));
         end if;
      end loop;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Text     : out String)
   is
   begin
      Read (Resource.File, Text'Size, Text'Address);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Value    : out Word_32)
   is
   begin
      Read (Resource.File, Value);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Value    : out Word_16)
   is
   begin
      Read (Resource.File, Value);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Value    : out Word_8)
   is
   begin
      Read (Resource.File, Value);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Value    : out Integer_32)
   is
   begin
      Read (Resource.File, Value);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Value    : out Integer_16)
   is
   begin
      Read (Resource.File, Value);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Value    : out Integer_8)
   is
   begin
      Read (Resource.File, Value);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Resource   : in out Chaos_Resource'Class;
      Max_Length : Natural)
      return String
   is
      Result : String (1 .. Max_Length);
      Length : Natural := 0;
      X      : Word_8;
   begin
      loop
         exit when Length = Max_Length;
         Read (Resource.File, X);
         exit when X = 0;
         Length := Length + 1;
         Result (Length) := Character'Val (X);
      end loop;
      return Result (1 .. Length);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32;
      Res_Ref  : out Resource_Reference)
   is
   begin
      Resource.Push_Offset (Offset);
      Resource.Get (Res_Ref);
      Resource.Pop_Offset;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32;
      Value    : out Word_32)
   is
   begin
      Resource.Push_Offset (Offset);
      Resource.Get (Value);
      Resource.Pop_Offset;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32;
      Value    : out Word_16)
   is
   begin
      Resource.Push_Offset (Offset);
      Resource.Get (Value);
      Resource.Pop_Offset;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32;
      Value    : out Word_8)
   is
   begin
      Resource.Push_Offset (Offset);
      Resource.Get (Value);
      Resource.Pop_Offset;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32;
      Value    : out Integer_32)
   is
   begin
      Resource.Push_Offset (Offset);
      Resource.Get (Value);
      Resource.Pop_Offset;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32;
      Value    : out Integer_16)
   is
   begin
      Resource.Push_Offset (Offset);
      Resource.Get (Value);
      Resource.Pop_Offset;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32;
      Value    : out Integer_8)
   is
   begin
      Resource.Push_Offset (Offset);
      Resource.Get (Value);
      Resource.Pop_Offset;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Resource   : in out Chaos_Resource'Class;
      Offset     : Word_32;
      Max_Length : Natural)
      return String
   is
   begin
      Resource.Push_Offset (Offset);
      declare
         Result : constant String :=
                    Resource.Get (Max_Length);
      begin
         Resource.Pop_Offset;
         return Result;
      end;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Unused   : out Unused_Bytes)
   is
   begin
      for I in Unused'Range loop
         Resource.Get (Unused (I));
      end loop;
   end Get;

   ------------------
   -- Has_Resource --
   ------------------

   function Has_Resource
     (Reference : Resource_Reference)
      return Boolean
   is
      S : constant String := To_String (Reference);
   begin
      return S /= "" and then Ada.Characters.Handling.To_Lower (S) /= "none";
   end Has_Resource;

   ----------
   -- Open --
   ----------

   procedure Open
     (Resource : in out Chaos_Resource'Class;
      Path     : String)
   is
      Required : constant String := Resource.Signature;
      Found    : String (Required'Range);
      Version  : String (1 .. 4);
      X        : Word_8;
   begin
      Chaos.Logging.Log
        ("RESOURCE", "loading: " & Path);
      Open (Resource.File, In_File, Path);

      if Resource.Has_Header then
         for I in Required'Range loop
            Resource.Get (X);
            Found (I) := Character'Val (X);
         end loop;
         if Found /= Required then
            raise Constraint_Error with
              "invalid signature: expected " & Required
              & "; found " & Found;
         elsif Found'Length = 4 then
            for I in Version'Range loop
               Resource.Get (X);
               Version (I) := Character'Val (X);
            end loop;

            Chaos.Logging.Log
              ("RESOURCE",
               "Signature: [" & Found & "] " & Version);
         end if;
      end if;

   end Open;

   ----------
   -- Open --
   ----------

   procedure Open
     (Resource : in out Chaos_Resource'Class;
      From     : Chaos_Resource'Class;
      Start    : WL.Binary_IO.Word_32;
      Length   : WL.Binary_IO.Word_32)
   is
      Required : constant String := Resource.Signature;
      Found    : String (Required'Range);
      X   : Word_8;
   begin
      Resource.File := View (From.File);
      Resource.Start := Start;
      Resource.Length := Length;
      Resource.Set_Offset (0);
      if Resource.Has_Header then
         for I in Required'Range loop
            Resource.Get (X);
            Found (I) := Character'Val (X);
         end loop;
         if Found /= Required then
            raise Constraint_Error with
              "invalid signature: expected " & Required
              & "; found " & Found;
         end if;
      end if;
   end Open;

   ----------------
   -- Pop_Offset --
   ----------------

   procedure Pop_Offset
     (Resource : in out Chaos_Resource'Class)
   is
      Offset : constant Word_32 := Resource.Saved_Offsets.Last_Element;
   begin
      Resource.Saved_Offsets.Delete_Last;
      Set_Offset (Resource.File, Offset);
   end Pop_Offset;

   -----------------
   -- Push_Offset --
   -----------------

   procedure Push_Offset
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32)
   is
   begin
      Resource.Saved_Offsets.Append (Current_Offset (Resource.File));
      Resource.Set_Offset (Offset);
   end Push_Offset;

   ----------------
   -- Set_Offset --
   ----------------

   procedure Set_Offset
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32)
   is
   begin
      Set_Offset (Resource.File, Resource.Start + Offset);
   end Set_Offset;

   ----------
   -- Skip --
   ----------

   procedure Skip
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32)
   is
   begin
      Set_Offset (Resource.File, Current_Offset (Resource.File) + Offset);
   end Skip;

   ------------------
   -- To_Reference --
   ------------------

   function To_Reference (Value : String) return Resource_Reference is
   begin
      return Reference : Resource_Reference :=
        (others => Character'Val (0))
      do
         for I in Value'Range loop
            Reference (I - Value'First + 1) := Value (I);
         end loop;
      end return;
   end To_Reference;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Resource_Reference) return String is
   begin
      for I in Value'Range loop
         if Character'Pos (Value (I)) = 0 then
            return String (Value (Value'First .. I - 1));
         end if;
      end loop;
      return String (Value);
   end To_String;

end Chaos.Resources;
