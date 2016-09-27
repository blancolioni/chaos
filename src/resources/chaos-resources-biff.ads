private with Ada.Containers.Vectors;

package Chaos.Resources.Biff is

   type Biff_Resource is
     new Chaos_Resource with private;

   procedure Open_Resource
     (Biff     : Biff_Resource'Class;
      Resource : in out Chaos_Resource'Class;
      Locator  : WL.Binary_IO.Word_32);

private

   type File_Entry is
      record
         Locator       : Word_32;
         Data_Start    : Word_32;
         Data_Length   : Word_32;
         Resource_Type : Word_16;
      end record;

   package File_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, File_Entry);

   type Tileset_Entry is
      record
         Locator       : Word_32;
         Data_Start    : Word_32;
         Tile_Count    : Word_32;
         Tile_Size     : Word_32;
         Resource_Type : Word_16;
      end record;

   package Tileset_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Tileset_Entry);

   type Biff_Resource is
     new Chaos_Resource with
      record
         File_Entries     : File_Entry_Vectors.Vector;
         Tileset_Entries  : Tileset_Entry_Vectors.Vector;
      end record;

   overriding function Signature
     (Biff : Biff_Resource)
      return String
   is ("BIFF");

   overriding procedure Load
     (Biff : in out Biff_Resource);

end Chaos.Resources.Biff;
