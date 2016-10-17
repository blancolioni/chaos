with Chaos.Resources.Vertices;

with Chaos.Features.Db;

package body Chaos.Features.Import is

   procedure Import_Polygon
     (Set          : in out Polygon_Vectors.Vector;
      Vertices     : Chaos.Resources.Vertices.Vertex_Vectors.Vector;
      First_Vertex : Positive;
      Vertex_Count : Natural);

   function Import_Container
     (Area            : Chaos.Resources.Area.Area_Resource'Class;
      Container_Index : Positive)
      return Chaos_Feature
   is
      Container : constant Chaos.Resources.Area.Container_Entry :=
                    Area.Containers.Element (Container_Index);

      procedure Create (Feature : in out Chaos_Feature_Record'Class)
      is null;

      procedure Configure (Feature : in out Chaos_Feature_Record'Class);

      ---------------
      -- Configure --
      ---------------

      procedure Configure (Feature : in out Chaos_Feature_Record'Class) is
         Poly_Vec : Polygon_Vectors.Vector;
      begin
         Import_Polygon
           (Poly_Vec,
            Area.Area_Vertices,
            Natural (Container.First_Vertex) + 1,
            Natural (Container.Vertex_Count));
         Feature.Polygon_Sets.Append (Poly_Vec);

         Feature.Bounding_Box :=
           (X1 => Integer (Container.Bounding_Box.Top_Left.X),
            X2 => Integer (Container.Bounding_Box.Bottom_Right.X),
            Y1 => Integer (Container.Bounding_Box.Top_Left.Y),
            Y2 => Integer (Container.Bounding_Box.Bottom_Right.Y));

         for I in 1 .. Container.Item_Count loop
            declare
               Index : constant Positive :=
                         Natural (Container.First_Item_Index)
                         + Positive (I);
            begin
               Feature.Contents.Append
                 (Chaos.Items.Create
                    (Chaos.Resources.To_String
                         (Area.Items.Element (Index).Resource)));
            end;
         end loop;

      end Configure;

      Feature : constant Chaos_Feature := Db.Create (Create'Access);
   begin

      Feature.Save_Object;
      Db.Update (Feature.Reference, Configure'Access);
      return Feature;

   end Import_Container;

   -----------------
   -- Import_Door --
   -----------------

   function Import_Door
     (Area       : Chaos.Resources.Area.Area_Resource'Class;
      Wed        : Chaos.Resources.Wed.Wed_Resource'Class;
      Area_Index : Positive;
      Wed_Index  : Positive)
      return Chaos_Feature
   is

      procedure Create (Feature : in out Chaos_Feature_Record'Class)
      is null;

      procedure Configure (Feature : in out Chaos_Feature_Record'Class);

      procedure Configure (Feature : in out Chaos_Feature_Record'Class) is
         Area_Door     : Chaos.Resources.Area.Door_Entry renames
                           Area.Doors.Element (Area_Index);
         Wed_Door      : Chaos.Resources.Wed.Door_Entry renames
                           Wed.Doors.Element (Wed_Index);
         Open_Polygons : Polygon_Vectors.Vector;

      begin

         for Polygon of Wed_Door.Polygons_Open loop
            declare
               Vertex_Count   : constant Natural :=
                                  Natural (Polygon.Vertex_Count);
               Feature_Poly   : Feature_Polygon  (1 .. Vertex_Count);
               Boundary_Index : Natural := 0;
            begin

               --  vertices are stpred by the resource in clockwise order,
               --  but we insist on anticlockwise boundaries
               for I in 1 .. Natural (Polygon.Vertex_Count) loop
                  declare
                     V : constant Chaos.Resources.Wed.Vertex :=
                           Wed.Vertices
                             (Natural (Polygon.Start_Vertex_Index) + I);
                  begin
                     Boundary_Index := Boundary_Index + 1;
                     Feature_Poly (Boundary_Index) :=
                       (Natural (V.X), Natural (V.Y));
                  end;
               end loop;
               Open_Polygons.Append (Feature_Poly);
            end;
         end loop;

         Feature.Polygon_Sets.Append (Open_Polygons);

         Import_Polygon
           (Feature.Sensitive_Areas, Area.Area_Vertices,
            Natural (Area_Door.First_Open_Vertex) + 1,
            Natural (Area_Door.Open_Vertex_Count));

         Feature.State := 1;
         Feature.Cursor_Index := 32;
      end Configure;

      Feature : constant Chaos_Feature := Db.Create (Create'Access);
   begin

      Feature.Save_Object;
      Db.Update (Feature.Reference, Configure'Access);
      return Feature;

   end Import_Door;

   --------------------
   -- Import_Polygon --
   --------------------

   procedure Import_Polygon
     (Set          : in out Polygon_Vectors.Vector;
      Vertices     : Chaos.Resources.Vertices.Vertex_Vectors.Vector;
      First_Vertex : Positive;
      Vertex_Count : Natural)
   is
      Poly : Feature_Polygon (1 .. Vertex_Count);
   begin
      for Index in 1 .. Vertex_Count loop
         declare
            V : constant Chaos.Resources.Vertices.Vertex :=
                  Vertices.Element (First_Vertex + Index - 1);
         begin
            Poly (Index) := (Natural (V.X), Natural (V.Y));
         end;
      end loop;
      Set.Append (Poly);
   end Import_Polygon;

   -------------------
   -- Import_Region --
   -------------------

   function Import_Region
     (Area         : Chaos.Resources.Area.Area_Resource'Class;
      Region_Index : Positive)
      return Chaos_Feature
   is
      Region  : Chaos.Resources.Area.Region_Entry renames
                  Area.Regions (Region_Index);

      procedure Create (Feature : in out Chaos_Feature_Record'Class)
      is null;

      procedure Configure (Feature : in out Chaos_Feature_Record'Class);

      ---------------
      -- Configure --
      ---------------

      procedure Configure (Feature : in out Chaos_Feature_Record'Class) is
         Poly_Vec : Polygon_Vectors.Vector;
      begin
         Import_Polygon
           (Poly_Vec,
            Area.Area_Vertices,
            Natural (Region.First_Vertex) + 1,
            Natural (Region.Vertex_Count));
         Feature.Polygon_Sets.Append (Poly_Vec);

         if Natural (Region.Region_Type) = 2 then
            Feature.Travel := True;
            Feature.Destination := Region.Destination_Area;
            Feature.Destination_Entrance := Region.Destination_Entrance;
         end if;

         Feature.Bounding_Box :=
           (X1 => Integer (Region.Bounding_Box.Top_Left.X),
            X2 => Integer (Region.Bounding_Box.Bottom_Right.X),
            Y1 => Integer (Region.Bounding_Box.Top_Left.Y),
            Y2 => Integer (Region.Bounding_Box.Bottom_Right.Y));

         Feature.Cursor_Index :=
           Natural (Region.Cursor_Index) + 1;

      end Configure;

      Feature : constant Chaos_Feature := Db.Create (Create'Access);
   begin

      Feature.Save_Object;
      Db.Update (Feature.Reference, Configure'Access);
      return Feature;

   end Import_Region;

end Chaos.Features.Import;
