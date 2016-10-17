package body Chaos.Resources.Vertices is

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Offset   : in     WL.Binary_IO.Word_32;
      Count    : in     WL.Binary_IO.Word_32;
      Vector   : in out Vertex_Vectors.Vector)
   is
   begin
      Vector.Clear;
      Resource.Set_Offset (Offset);
      for I in 1 .. Count loop
         declare
            V : Vertex;
         begin
            Resource.Get (V.X);
            Resource.Get (V.Y);
            Vector.Append (V);
         end;
      end loop;

   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Resource : in out Chaos_Resource'Class;
      Box      : out Rectangle)
   is
   begin
      Resource.Get (Box.Top_Left.X);
      Resource.Get (Box.Top_Left.Y);
      Resource.Get (Box.Bottom_Right.X);
      Resource.Get (Box.Bottom_Right.Y);
   end Get;

end Chaos.Resources.Vertices;
