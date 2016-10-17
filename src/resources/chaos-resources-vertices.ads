with Ada.Containers.Vectors;

package Chaos.Resources.Vertices is

   type Vertex is
      record
         X, Y : WL.Binary_IO.Word_16;
      end record;

   package Vertex_Vectors is
     new Ada.Containers.Vectors (Positive, Vertex);

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Offset   : in     WL.Binary_IO.Word_32;
      Count    : in     WL.Binary_IO.Word_32;
      Vector   : in out Vertex_Vectors.Vector);

   type Rectangle is
      record
         Top_Left     : Vertex;
         Bottom_Right : Vertex;
      end record;

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Box      : out Rectangle);

end Chaos.Resources.Vertices;
