with Lith.Objects.Interfaces;

with Chaos.Logging;

with Chaos.Creatures;
with Chaos.Creatures.Import;

with Chaos.Game;

package body Chaos.Areas.Primitives is

   function Evaluate_Create_Actor
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Distance
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Exists
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Match_Object
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Exists
     (Object : Lith.Objects.Object)
      return Boolean;

   -----------------------
   -- Create_Primitives --
   -----------------------

   procedure Create_Primitives is
      use Lith.Objects.Interfaces;
   begin
      Define_Function ("chaos-create-actor", Evaluate_Create_Actor'Access);
      Define_Function ("chaos-distance", Evaluate_Distance'Access);
      Define_Function ("chaos-object-exists", Evaluate_Exists'Access);
      Define_Function ("chaos-match-object", Evaluate_Match_Object'Access);
   end Create_Primitives;

   ---------------------------
   -- Evaluate_Create_Actor --
   ---------------------------

   function Evaluate_Create_Actor
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Area     : constant Chaos.Areas.Chaos_Area :=
                   Chaos.Areas.Chaos_Area
                     (Chaos.Objects.To_Object
                        (Store.Argument (1)));
      Code     : constant String :=
                   Lith.Objects.Get_Name
                     (Lith.Objects.To_Symbol
                        (Store.Argument (2)));
      X        : constant Integer :=
                   Lith.Objects.To_Integer (Store.Argument (3));
      Y        : constant Integer :=
                   Lith.Objects.To_Integer (Store.Argument (4));
      Facing   : constant Integer :=
                   Lith.Objects.To_Integer (Store.Argument (5));
      Creature : constant Chaos.Creatures.Chaos_Creature :=
                   (if Chaos.Creatures.Exists (Code)
                    then Chaos.Creatures.Get (Code)
                    else Chaos.Creatures.Import.Import_Creature (Code));
      Actor    : constant Chaos.Actors.Chaos_Actor :=
                   Chaos.Actors.Create_Actor
                     (Creature, Area,
                      Area.To_Square ((X, Y)),
                      Chaos.Locations.Orientation'Val
                        (Facing / 2));
   begin
      Chaos.Logging.Log
        ("AREA", "created " & Code & " at" & X'Img & Y'Img);
      return Actor.To_Expression;
   end Evaluate_Create_Actor;

   -----------------------
   -- Evaluate_Distance --
   -----------------------

   function Evaluate_Distance
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is

      Area : constant Chaos.Areas.Chaos_Area :=
               Chaos.Game.Current_Game.Area;

      function Get_Loc (Value : Lith.Objects.Object)
                        return Chaos.Locations.Square_Location;

      -------------
      -- Get_Loc --
      -------------

      function Get_Loc (Value : Lith.Objects.Object)
                        return Chaos.Locations.Square_Location
      is
         Object : constant Chaos.Objects.Chaos_Object :=
                    Chaos.Objects.To_Object (Value);
      begin
         if Object.all in Chaos.Creatures.Chaos_Creature_Record'Class then
            return Area.Actor
              (Chaos.Creatures.Chaos_Creature (Object)).Location;
         elsif Object.all in Chaos.Actors.Chaos_Actor_Record'Class then
            return Chaos.Actors.Chaos_Actor (Object).Location;
         else
            raise Constraint_Error with
              "expected a creature or an actor, but found "
              & Object.Display_Name;
         end if;
      end Get_Loc;

   begin
      if not Exists (Store.Argument (1))
        or else not Exists (Store.Argument (2))
      then
         return Lith.Objects.To_Object (Integer'(9_999));
      else
         declare
            Loc_1 : constant Chaos.Locations.Square_Location :=
                      Get_Loc (Store.Argument (1));
            Loc_2 : constant Chaos.Locations.Square_Location :=
                      Get_Loc (Store.Argument (2));
         begin
            return Lith.Objects.To_Object
              (Chaos.Locations.Minimum_Distance (Loc_1, Loc_2));
         end;
      end if;
   end Evaluate_Distance;

   ---------------------
   -- Evaluate_Exists --
   ---------------------

   function Evaluate_Exists
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      return Lith.Objects.To_Object (Exists (Store.Argument (1)));
   end Evaluate_Exists;

   ---------------------------
   -- Evaluate_Match_Object --
   ---------------------------

   function Evaluate_Match_Object
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      function Get (Arg_Index : Positive) return Natural
      is (Lith.Objects.To_Integer (Store.Argument (Arg_Index)));

      Properties : constant Chaos.Creatures.Creature_Match :=
                     (Enemy_Ally => Get (1),
                      General    => Get (2),
                      Race       => Get (3),
                      Class      => Get (4),
                      Specific   => Get (5),
                      Gender     => Get (6),
                      Alignment  => Get (7));

      function Match (Actor : Chaos.Actors.Chaos_Actor) return Boolean;

      -----------
      -- Match --
      -----------

      function Match (Actor : Chaos.Actors.Chaos_Actor) return Boolean is
      begin
         return Actor.Creature.Match (Properties);
      end Match;

      Result : constant Chaos.Actors.Chaos_Actor :=
                 Chaos.Game.Current_Game.Area.Find_Matching_Actor
                   (Match'Access);

      use type Chaos.Actors.Chaos_Actor;

   begin
      if Result = null then
         return Lith.Objects.False_Value;
      else
         return Result.To_Expression;
      end if;
   end Evaluate_Match_Object;

   ------------
   -- Exists --
   ------------

   function Exists
     (Object : Lith.Objects.Object)
      return Boolean
   is
      use Lith.Objects;
      use type Chaos.Objects.Chaos_Object;

      Item : constant Chaos.Objects.Chaos_Object :=
               (if Object = False_Value
                or else Object = Nil
                or else not Chaos.Objects.Is_Object (Object)
                then null
                else Chaos.Objects.To_Object (Object));
   begin
      if Item = null then
         return False;
      elsif Item.all in Chaos.Creatures.Chaos_Creature_Record'Class then
         return Chaos.Game.Current_Game.Area.Has_Actor
           (Chaos.Creatures.Chaos_Creature (Item));
      else
         return False;
      end if;
   end Exists;

end Chaos.Areas.Primitives;
