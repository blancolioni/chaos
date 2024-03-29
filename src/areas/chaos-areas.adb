with Ada.Characters.Handling;
with Ada.Numerics.Elementary_Functions;

with Chaos.Areas.Db;

package body Chaos.Areas is

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

   -----------
   -- Actor --
   -----------

   function Actor
     (Area  : Chaos_Area_Record'Class;
      Index : Positive)
      return Chaos.Actors.Chaos_Actor
   is
   begin
      return Area.Actors.Element (Index);
   end Actor;

   -----------
   -- Actor --
   -----------

   function Actor
     (Area     : Chaos_Area_Record'Class;
      Creature : Chaos.Creatures.Chaos_Creature)
      return Chaos.Actors.Chaos_Actor
   is
      use type Chaos.Creatures.Chaos_Creature;
   begin
      for Actor of Area.Actors loop
         if Actor.Creature = Creature then
            return Actor;
         end if;
      end loop;
      raise Constraint_Error with
        Area.Identifier & ": no actor found with creature "
        & Creature.Identifier;
   end Actor;

   -----------------
   -- Actor_Count --
   -----------------

   function Actor_Count
     (Area     : Chaos_Area_Record'Class)
      return Natural
   is
   begin
      return Area.Actors.Last_Index;
   end Actor_Count;

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

   -----------------------
   -- After_Actor_Moved --
   -----------------------

   procedure After_Actor_Moved
     (Area  : Chaos_Area_Record'Class;
      Actor : Chaos.Actors.Chaos_Actor;
      From  : Chaos.Locations.Square_Location)
   is

      Position : constant Actor_Vectors.Cursor :=
                   Area.Actors.Find (Actor);

      procedure Move (A : in out Chaos_Area_Record'Class);

      ----------
      -- Move --
      ----------

      procedure Move (A : in out Chaos_Area_Record'Class) is
      begin
         A.Squares (A.To_Square_Index (From)).Actor := null;
         A.Squares (A.To_Square_Index (Actor.Location)).Actor := Actor;
      end Move;

   begin
      if Actor_Vectors.Has_Element (Position) then
         Db.Update (Area.Reference, Move'Access);
      else
         raise Constraint_Error with
           "After_Actor_Moved: no such actor " & Actor.Identifier
           & " in area " & Area.Identifier;
      end if;
   end After_Actor_Moved;

   ------------
   -- Create --
   ------------

   procedure Create
     (Area                      : in out Chaos_Area_Record'Class;
      Identity                  : String;
      Pixel_Width, Pixel_Height : Natural)
   is
      use Ada.Numerics.Elementary_Functions;
      Diagonal : constant Float :=
                   Sqrt (Float (Pixel_Width / Pixels_Per_Square) ** 2
                         + Float (Pixel_Height / Pixels_Per_Square) ** 2);
      Init_Square : constant Square_Type :=
                      (Actor           => null,
                       Feature         => null,
                       Passable        => False,
                       Transparent     => False);
   begin
      Area.Initialize (Identity);
      Area.Pixel_Width := Pixel_Width;
      Area.Pixel_Height := Pixel_Height;
      Area.Squares_Across := Natural (Diagonal + 1.0);
      Area.Squares_Down := Natural (Diagonal + 1.0);

      for Y in 1 .. Area.Squares_Down loop
         for X in 1 .. Area.Squares_Across loop
            Area.Squares.Append (Init_Square);
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

   ---------------------
   -- Entrance_Square --
   ---------------------

   function Entrance_Square
     (Area  : Chaos_Area_Record'Class;
      Name  : String)
      return Chaos.Locations.Square_Location
   is
      function Proper_Name (S : String) return String;

      -----------------
      -- Proper_Name --
      -----------------

      function Proper_Name (S : String) return String is
         use Ada.Characters.Handling;
         Result : String := S;
      begin
         for I in Result'Range loop
            if Is_Digit (Result (I)) then
               null;
            elsif Is_Upper (Result (I)) then
               Result (I) := To_Lower (Result (I));
            elsif Is_Lower (Result (I)) then
               null;
            else
               return Result (1 .. I - 1);
            end if;
         end loop;
         return Result;
      end Proper_Name;

      N : constant String := Proper_Name (Name);
   begin
      for Entrance of Area.Entrances loop
         if Proper_Name (Entrance.Name) = N then
            return Entrance.Square;
         end if;
      end loop;
      raise Constraint_Error with
        "Entrance_Square: no such entrance " & Name
        & " in area " & Area.Identifier;
   end Entrance_Square;

   ------------
   -- Exists --
   ------------

   function Exists
     (Identity : String)
      return Boolean
   is
   begin
      return Db.Exists (Identity);
   end Exists;

   -------------
   -- Feature --
   -------------

   function Feature
     (Area  : Chaos_Area_Record'Class;
      Index : Positive)
      return Chaos.Features.Chaos_Feature
   is
   begin
      return Area.Features.Element (Index);
   end Feature;

   -------------
   -- Feature --
   -------------

   function Feature
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Features.Chaos_Feature
   is
   begin
      return Area.Squares.Element (Area.To_Square_Index (Location)).Feature;
   end Feature;

   -------------------
   -- Feature_Count --
   -------------------

   function Feature_Count
     (Area : Chaos_Area_Record'Class)
      return Natural
   is
   begin
      return Area.Features.Last_Index;
   end Feature_Count;

   -------------------------
   -- Find_Matching_Actor --
   -------------------------

   function Find_Matching_Actor
     (Area    : Chaos_Area_Record'Class;
      Test    : not null access
        function (Actor : Chaos.Actors.Chaos_Actor) return Boolean)
      return Chaos.Actors.Chaos_Actor
   is
   begin
      for Actor of Area.Actors loop
         if Test (Actor) then
            return Actor;
         end if;
      end loop;
      return null;
   end Find_Matching_Actor;

   ---------------
   -- Find_Path --
   ---------------

   overriding function Find_Path
     (Area   : Chaos_Area_Record;
      Start  : Chaos.Locations.Square_Location;
      Finish : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path
   is
      function OK
        (Location : Chaos.Locations.Square_Location)
         return Boolean
      is (not Area.Has_Actor (Location) and then Area.Passable (Location));
   begin
      return Chaos.Locations.Find_Path
        (Start, Finish, Area.Squares_Across - 1, Area.Squares_Down - 1,
         OK'Access);
   end Find_Path;

   ---------
   -- Get --
   ---------

   function Get
     (Identity : String)
      return Chaos_Area
   is
   begin
      return Db.Get (Identity);
   end Get;

   ---------------
   -- Has_Actor --
   ---------------

   function Has_Actor
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Boolean
   is
      use type Chaos.Actors.Chaos_Actor;
      Index : constant Positive := Area.To_Square_Index (Location);
   begin
      return Area.Squares.Element (Index).Actor /= null;
   end Has_Actor;

   ---------------
   -- Has_Actor --
   ---------------

   function Has_Actor
     (Area     : Chaos_Area_Record'Class;
      Creature : Chaos.Creatures.Chaos_Creature)
      return Boolean
   is
      use type Chaos.Creatures.Chaos_Creature;
   begin
      for Actor of Area.Actors loop
         if Actor.Creature = Creature then
            return True;
         end if;
      end loop;
      return False;
   end Has_Actor;

   -----------------
   -- Has_Feature --
   -----------------

   function Has_Feature
     (Area     : Chaos_Area_Record'Class;
      Location : Chaos.Locations.Square_Location)
      return Boolean
   is
      use type Chaos.Features.Chaos_Feature;
   begin
      return Area.Squares.Element (Area.To_Square_Index (Location)).Feature
        /= null;
   end Has_Feature;

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

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (Area       : in out Chaos_Area_Record;
      Mark_Value : not null access
        procedure (Value : in out Lith.Objects.Object))
   is
      procedure Mark_Actor
        (Actor : in out Chaos.Actors.Chaos_Actor_Record'Class);

      ----------------
      -- Mark_Actor --
      ----------------

      procedure Mark_Actor
        (Actor : in out Chaos.Actors.Chaos_Actor_Record'Class)
      is
      begin
         Actor.Mark (Mark_Value);
      end Mark_Actor;

   begin
      Chaos.Objects.Root_Chaos_Object_Record (Area).Mark (Mark_Value);
      for Actor of Area.Actors loop
         Actor.Update (Mark_Actor'Access);
      end loop;
   end Mark;

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
      return Memor.Memor_Database
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

   --------------------
   -- Part_Of_Battle --
   --------------------

   function Part_Of_Battle
     (Area  : Chaos_Area_Record'Class;
      Actor : Chaos.Actors.Chaos_Actor)
      return Boolean
   is
      pragma Unreferenced (Area, Actor);
   begin
      return False;
   end Part_Of_Battle;

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

   ------------------
   -- Remove_Actor --
   ------------------

   procedure Remove_Actor
     (Area     : Chaos_Area_Record'Class;
      Actor    : Chaos.Actors.Chaos_Actor)
   is

      Position : Actor_Vectors.Cursor :=
                   Area.Actors.Find (Actor);

      procedure Remove (A : in out Chaos_Area_Record'Class);

      ------------
      -- Remove --
      ------------

      procedure Remove (A : in out Chaos_Area_Record'Class) is
      begin
         A.Actors.Delete (Position);
         A.Squares (A.To_Square_Index (Actor.Location)).Actor := null;
      end Remove;

   begin
      if Actor_Vectors.Has_Element (Position) then
         Db.Update (Area.Reference, Remove'Access);
      else
         raise Constraint_Error with
           "no such actor " & Actor.Identifier
           & " in area " & Area.Identifier;
      end if;
   end Remove_Actor;

   --------------------------
   -- Scan_Matching_Actors --
   --------------------------

   procedure Scan_Matching_Actors
     (Area    : Chaos_Area_Record'Class;
      Test    : not null access
        function (Actor : Chaos.Actors.Chaos_Actor) return Boolean;
      Process : not null access
        procedure (Actor : Chaos.Actors.Chaos_Actor))
   is
   begin
      for Actor of Area.Actors loop
         if Test (Actor) then
            Process (Actor);
         end if;
      end loop;
   end Scan_Matching_Actors;

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
              and then Area.Visible
                (Looker.Visible_Range (Area.Visibility),
                 Looker.Location, Actor.Location)
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
              and then Area.Visible
                (Actor.Visible_Range (Area.Visibility),
                 Actor.Location, Lookee.Location)
            then
               Process (Actor);
            end if;
         end;
      end loop;
   end Scan_Visible_To_Actors;

   --------------------
   -- Squares_Across --
   --------------------

   function Squares_Across (Area : Chaos_Area_Record'Class) return Natural is
   begin
      return Area.Squares_Across;
   end Squares_Across;

   ------------------
   -- Squares_Down --
   ------------------

   function Squares_Down (Area : Chaos_Area_Record'Class) return Natural is
   begin
      return Area.Squares_Down;
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
--        use Ada.Numerics.Elementary_Functions;
--        Root_2  : constant Float := Sqrt (2.0) / 2.0;
--        Square_X : constant Float :=
--                     Float (Square_Location.X - Area.Squares_Across / 2);
--        Square_Y : constant Float :=
--                     Float (Square_Location.Y - Area.Squares_Down / 2);
--        Rot_X    : constant Float :=
--                     Square_X * Root_2 + Square_Y * Root_2;
--        Rot_Y    : constant Float :=
--                     Square_Y * Root_2 - Square_X * Root_2;
--        Pixel_X  : constant Integer :=
--                     Integer (Rot_X * Float (Pixels_Per_Square))
--                     + Area.Pixel_Width / 2;
--        Pixel_Y  : constant Integer :=
--                     Area.Pixel_Height / 2
--                     - Integer (Rot_Y * Float (Pixels_Per_Square));
      Pixel_X : constant Natural := Square_Location.X * Pixels_Per_Square;
      Pixel_Y : constant Natural := Square_Location.Y * Pixels_Per_Square;
   begin
      return (Pixel_X, Pixel_Y);
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
--        use Ada.Numerics.Elementary_Functions;
--        Root_2  : constant Float := Sqrt (2.0) / 2.0;
--        Pixel_X : constant Float :=
--                    Float (Pixel_Location.X - Area.Pixels_Across / 2);
--        Pixel_Y : constant Float :=
--                    Float (Area.Pixels_Down / 2 - Pixel_Location.Y);
--        Rot_X   : constant Float :=
--                    Pixel_X * Root_2 - Pixel_Y * Root_2;
--        Rot_Y   : constant Float :=
--                    Pixel_Y * Root_2 + Pixel_X * Root_2;
--        Square_X : constant Natural :=
--                     (Integer (Rot_X))
--                     / Pixels_Per_Square
--                       + Area.Squares_Across / 2;
--        Square_Y : constant Natural :=
--                     (Integer (Rot_Y))
--                     / Pixels_Per_Square
--                       + Area.Squares_Down / 2;
      Square_X : constant Natural :=
                   Pixel_Location.X / Pixels_Per_Square;
      Square_Y : constant Natural :=
                   Pixel_Location.Y / Pixels_Per_Square;
   begin
      return (Square_X, Square_Y);
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

   ------------------------
   -- To_Square_Location --
   ------------------------

   function To_Square_Location
     (Area         : Chaos_Area_Record'Class;
      Square_Index : Positive)
      return Chaos.Locations.Square_Location
   is
   begin
      return ((Square_Index - 1) mod Area.Squares_Across,
              (Square_Index - 1) / Area.Squares_Across);
   end To_Square_Location;

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

   ------------------
   -- Valid_Square --
   ------------------

   function Valid_Square
     (Area            : Chaos_Area_Record'Class;
      Square_Location : Chaos.Locations.Square_Location)
      return Boolean
   is
      Pix : constant Chaos.Locations.Pixel_Location :=
              Area.To_Pixels (Square_Location);
   begin
      return Pix.X in 0 .. Area.Pixel_Width - 1
        and then Pix.Y in 0 .. Area.Pixel_Height - 1;
   end Valid_Square;

   -------------
   -- Visible --
   -------------

   function Visible
     (Area     : Chaos_Area_Record'Class;
      Max      : Natural;
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
      return Chaos.Locations.Length (Path) in 1 .. Max;
   end Visible;

end Chaos.Areas;
