private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

package Chaos.Resources.Tlk is

   type Tlk_Resource is
     new Chaos_Resource with private;

private

   type String_Entry is
      record
         Flags    : Word_16;
         Sound    : Resource_Reference;
         Volume   : Word_32;
         Pitch    : Word_32;
         Text     : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package String_Entry_Vectors is
     new Ada.Containers.Vectors (Natural, String_Entry);

   type Tlk_Resource is
     new Chaos_Resource with
      record
         String_Entries : String_Entry_Vectors.Vector;
      end record;

   overriding function Signature
     (Resource : Tlk_Resource)
      return String
   is ("TLK ");

   overriding procedure Load
     (Resource : in out Tlk_Resource);

end Chaos.Resources.Tlk;
