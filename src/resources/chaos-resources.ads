private with Ada.Containers.Doubly_Linked_Lists;

with WL.Binary_IO;

with Chaos.Objects;

package Chaos.Resources is

   type Unused_Bytes is array (Positive range <>) of WL.Binary_IO.Word_8;

   type Resource_Reference is new String (1 .. 8);
   subtype String_Reference is WL.Binary_IO.Word_32;

   function To_Reference (Value : String) return Resource_Reference;
   function To_String (Value : Resource_Reference) return String;

   function Has_Resource
     (Reference : Resource_Reference)
      return Boolean;

   type Script_Level is
     (Override_Script,
      Area_Script,
      Specific_Script,
      Class_Script,
      Race_Script,
      General_Script,
      Default_Script);

   type Script_Array is array (Script_Level) of Resource_Reference;

   type Resource_Color is
      record
         B, G, R, A : WL.Binary_IO.Word_8;
      end record;

   type Resource_Palette is array (WL.Binary_IO.Word_8) of Resource_Color;

   type Resource_Type is private;

   Area_Resource       : constant Resource_Type;
   Bmp_Resource        : constant Resource_Type;
   Bam_Resource        : constant Resource_Type;
   Creature_Resource   : constant Resource_Type;
   Dialog_Resource     : constant Resource_Type;
   Identifier_Resource : constant Resource_Type;
   Item_Resource       : constant Resource_Type;
   Script_Resource     : constant Resource_Type;
   Table_Resource      : constant Resource_Type;
   Tileset_Resource    : constant Resource_Type;
   Wed_Resource        : constant Resource_Type;

   function Extension
     (Resource : Resource_Type)
      return String;

   type Chaos_Resource is abstract tagged limited private;

   function Is_Text_Resource
     (Resource : Chaos_Resource)
      return Boolean
   is (False);

   function Name
     (Resource : Chaos_Resource'Class)
      return String;

   function Signature
     (Resource : Chaos_Resource)
      return String
      is abstract;

   procedure Load
     (Resource : in out Chaos_Resource)
   is abstract;

   function Has_Header
     (Resource : Chaos_Resource)
      return Boolean
   is (True);

   procedure Open
     (Resource  : in out Chaos_Resource'Class;
      Reference : Resource_Reference;
      Path      : String);

   procedure Open
     (Resource  : in out Chaos_Resource'Class;
      Reference : Resource_Reference;
      From      : Chaos_Resource'Class;
      Start     : WL.Binary_IO.Word_32;
      Length    : WL.Binary_IO.Word_32);

   procedure Close
     (Resource : in out Chaos_Resource'Class);

   function End_Of_Resource
     (Resource : Chaos_Resource'Class)
      return Boolean;

private

   use WL.Binary_IO;

   type Resource_Type is new Word_16;

   Bmp_Resource        : constant Resource_Type := 16#0001#;
   Bam_Resource        : constant Resource_Type := 16#03E8#;
   Wed_Resource        : constant Resource_Type := 16#03E9#;
   Tileset_Resource    : constant Resource_Type := 16#03EB#;
   Item_Resource       : constant Resource_Type := 16#03ED#;
   Script_Resource     : constant Resource_Type := 16#03EF#;
   Identifier_Resource : constant Resource_Type := 16#03F0#;
   Creature_Resource   : constant Resource_Type := 16#03F1#;
   Area_Resource       : constant Resource_Type := 16#03F2#;
   Dialog_Resource     : constant Resource_Type := 16#03F3#;
   Table_Resource      : constant Resource_Type := 16#03F4#;

   package Offset_Stacks is
     new Ada.Containers.Doubly_Linked_Lists (Word_32);

   type Chaos_Resource is abstract tagged limited
      record
         File          : WL.Binary_IO.File_Type;
         Reference     : Resource_Reference;
         Start         : Word_32 := 0;
         Length        : Word_32 := 0;
         Busy          : Boolean := False;
         Saved_Offsets : Offset_Stacks.List;
      end record;

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Unused   : out Unused_Bytes);

   procedure Set_Offset
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32);

   procedure Push_Offset
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32);

   procedure Pop_Offset
     (Resource : in out Chaos_Resource'Class);

   procedure Skip
     (Resource : in out Chaos_Resource'Class;
      Offset   : Word_32);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Res_Ref  : out Resource_Reference);

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Text     : out String);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Value    : out Word_32);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Value    : out Word_16);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Value    : out Word_8);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Value    : out Integer_32);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Value    : out Integer_16);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Value    : out Integer_8);

   function Get (Resource : in out Chaos_Resource'Class;
                 Max_Length : Natural)
                 return String;

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Offset   : Word_32;
                  Res_Ref  : out Resource_Reference);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Offset   : Word_32;
                  Value    : out Word_32);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Offset   : Word_32;
                  Value    : out Word_16);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Offset   : Word_32;
                  Value    : out Word_8);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Offset   : Word_32;
                  Value    : out Integer_32);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Offset   : Word_32;
                  Value    : out Integer_16);

   procedure Get (Resource : in out Chaos_Resource'Class;
                  Offset   : Word_32;
                  Value    : out Integer_8);

   function Get (Resource : in out Chaos_Resource'Class;
                 Offset   : Word_32;
                 Max_Length : Natural)
                 return String;

end Chaos.Resources;
