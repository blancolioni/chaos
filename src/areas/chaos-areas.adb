with Chaos.Areas.Db;

package body Chaos.Areas is

   Pixels_Per_Square : constant := 16;

   -----------
   -- Actor --
   -----------

   function Actor
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Actors.Chaos_Actor
   is
      Index : constant Positive := Area.To_Square_Index (Location);
   begin
      return Area.Squares.Element (Index).Actor;
   end Actor;

   ---------------
   -- Add_Actor --
   ---------------

   procedure Add_Actor
     (Area     : Chaos_Area_Record'Class;
      Actor    : Chaos.Actors.Chaos_Actor)
   is
      procedure Add (A : in out Chaos_Area_Record'Class);

      ---------
      -- Add --
      ---------

      procedure Add (A : in out Chaos_Area_Record'Class) is
      begin
         A.Actors.Append (Actor);
         A.Squares (Area.To_Square_Index (Actor.Location)).Actor :=
           Actor;
      end Add;

   begin
      Db.Update (Area.Reference, Add'Access);
   end Add_Actor;

   ------------
   -- Create --
   ------------

   procedure Create
     (Area                      : in out Chaos_Area_Record'Class;
      Identity                  : String;
      Pixel_Width, Pixel_Height : Natural)
   is
   begin
      Area.Initialize (Identity);
      Area.Pixel_Width := Pixel_Width;
      Area.Pixel_Height := Pixel_Height;
      for Y in 1 .. Area.Squares_Down loop
         for X in 1 .. Area.Squares_Across loop
            Area.Squares.Append ((null, True, True));
         end loop;
      end loop;
   end Create;

   --------------------
   -- Current_Battle --
   --------------------

   overriding function Current_Battle
     (Area : Chaos_Area_Record)
      return Boolean
   is
      pragma Unreferenced (Area);
   begin
      return False;
   end Current_Battle;

   ---------------
   -- Find_Path --
   ---------------

   overriding function Find_Path
     (Area   : Chaos_Area_Record;
      Start  : Chaos.Locations.Square_Location;
      Finish : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path
   is
      use type Chaos.Actors.Chaos_Actor;
      function OK
        (Location : Chaos.Locations.Square_Location)
         return Boolean
      is (Area.Actor (Location) = null and then Area.Passable (Location));
   begin
      return Chaos.Locations.Find_Path
        (Start, Finish, Area.Squares_Across - 1, Area.Squares_Down - 1,
         OK'Access);
   end Find_Path;

   ------------
   -- Images --
   ------------

   function Images
     (Area : Chaos_Area_Record'Class)
      return Chaos.Images.Chaos_Image_Container
   is
   begin
      return Area.Images;
   end Images;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path
   is
      Result : Chaos.Locations.Square_Path;
      Max_X  : constant Natural := Area.Squares_Across - 1;
      Max_Y  : constant Natural := Area.Squares_Down - 1;
   begin
      for X in -1 .. 1 loop
         for Y in -1 .. 1 loop
            if X /= Location.X
              and then Y /= Location.Y
              and then Location.X + X in 0 .. Max_X
              and then Location.Y + Y in 0 .. Max_Y
            then
               Chaos.Locations.Append
                 (Result, (Location.X + X, Location.Y + Y));
            end if;
         end loop;
      end loop;
      return Result;
   end Neighbours;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Area_Record)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

   --------------
   -- Passable --
   --------------

   function Passable
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Boolean
   is
   begin
      return Area.Squares (Area.To_Square_Index (Location)).Passable;
   end Passable;

   -------------------
   -- Pixels_Across --
   -------------------

   function Pixels_Across (Area : Chaos_Area_Record'Class) return Natural is
   begin
      return Area.Pixel_Width;
   end Pixels_Across;

   -----------------
   -- Pixels_Down --
   -----------------

   function Pixels_Down (Area : Chaos_Area_Record'Class) return Natural is
   begin
      return Area.Pixel_Height;
   end Pixels_Down;

   -------------------------
   -- Scan_Visible_Actors --
   -------------------------

   procedure Scan_Visible_Actors
     (Area    : Chaos_Area_Record'Class;
      Looker  : Chaos.Actors.Chaos_Actor;
      Process : not null access
        procedure (Actor : Chaos.Actors.Chaos_Actor))
   is
      Max_Range : constant Natural :=
                    Looker.Visible_Range (Area.Visibility);
   begin
      for Actor of Area.Actors loop
         declare
            Distance : constant Natural :=
                         Chaos.Locations.Minimum_Distance
                           (Looker.Location, Actor.Location);
         begin
            if Distance <= Max_Range
              and then Area.Visible (Looker.Location, Actor.Location)
            then
               Process (Actor);
            end if;
         end;
      end loop;
   end Scan_Visible_Actors;

   ----------------------------
   -- Scan_Visible_To_Actors --
   ----------------------------

   procedure Scan_Visible_To_Actors
     (Area    : Chaos_Area_Record'Class;
      Lookee  : Chaos.Actors.Chaos_Actor;
      Process : not null access
        procedure (Actor : Chaos.Actors.Chaos_Actor))
   is
   begin
      for Actor of Area.Actors loop
         declare
            Max_Range : constant Natural :=
                          Actor.Visible_Range (Area.Visibility);
            Distance  : constant Natural :=
                         Chaos.Locations.Minimum_Distance
                           (Actor.Location, Lookee.Location);
         begin
            if Distance <= Max_Range
              and then Area.Visible (Actor.Location, Lookee.Location)
            then
               Process (Actor);
            end if;
         end;
      end loop;
   end Scan_Visible_To_Actors;

   ------------
   -- Script --
   ------------

   function Script
     (Area : Chaos_Area_Record'class)
      return Chaos.Expressions.Chaos_Expression
   is
   begin
      return Area.Script;
   end Script;

   --------------------
   -- Squares_Across --
   --------------------

   function Squares_Across (Area : Chaos_Area_Record'Class) return Natural is
   begin
      return Area.Pixel_Width / Pixels_Per_Square;
   end Squares_Across;

   ------------------
   -- Squares_Down --
   ------------------

   function Squares_Down (Area : Chaos_Area_Record'Class) return Natural is
   begin
      return Area.Pixel_Height / Pixels_Per_Square;
   end Squares_Down;

   ----------------
   -- Tile_Index --
   ----------------

   function Tile_Index
     (Area           : Chaos_Area_Record'Class;
      Tile_X, Tile_Y : Positive)
      return Positive
   is
      Index : constant Positive :=
                (Tile_Y - 1) * Area.Tiles_Across + Tile_X;
   begin
      return Area.Tiles.Element (Index).Tile_Index;
   end Tile_Index;

   ------------------
   -- Tiles_Across --
   ------------------

   function Tiles_Across (Area : Chaos_Area_Record'Class) return Natural is
   begin
      return Area.Pixel_Width / 64;
   end Tiles_Across;

   ----------------
   -- Tiles_Down --
   ----------------

   function Tiles_Down (Area : Chaos_Area_Record'Class) return Natural is
   begin
      return Area.Pixel_Height / 64;
   end Tiles_Down;

   ---------------
   -- To_Pixels --
   ---------------

   function To_Pixels
     (Area            : Chaos_Area_Record'Class;
      Square_Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Pixel_Location
   is
      pragma Unreferenced (Area);
   begin
      return ((Square_Location.X - 1) * Pixels_Per_Square
              + 1 + Pixels_Per_Square / 2,
              (Square_Location.Y - 1) * Pixels_Per_Square
              + 1 + Pixels_Per_Square / 2);
   end To_Pixels;

   ---------------
   -- To_Square --
   ---------------

   function To_Square
     (Area           : Chaos_Area_Record'Class;
      Pixel_Location : Chaos.Locations.Pixel_Location)
      return Chaos.Locations.Square_Location
   is
      pragma Unreferenced (Area);
   begin
      return (Pixel_Location.X / Pixels_Per_Square,
              Pixel_Location.Y / Pixels_Per_Square);
   end To_Square;

   ---------------------
   -- To_Square_Index --
   ---------------------

   function To_Square_Index
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Positive
   is
   begin
      return Location.X + Area.Squares_Across * Location.Y + 1;
   end To_Square_Index;

   -----------------
   -- Transparent --
   -----------------

   function Transparent
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Boolean
   is
   begin
      return Area.Squares (Area.To_Square_Index (Location)).Transparent;
   end Transparent;

   -------------
   -- Visible --
   -------------

   function Visible
     (Area     : Chaos_Area_Record'Class;
      Square_1 : Chaos.Locations.Square_Location;
      Square_2 : Chaos.Locations.Square_Location)
      return Boolean
   is
      function OK
        (Location : Chaos.Locations.Square_Location)
         return Boolean
      is (Area.Transparent (Location));

      Path : constant Chaos.Locations.Square_Path :=
               Chaos.Locations.Straight_Line_Path
                 (Square_1, Square_2,
                  Area.Squares_Across, Area.Squares_Down,
                  OK'Access);
   begin
      return Chaos.Locations.Length (Path) > 0;
   end Visible;

end Chaos.Areas;
