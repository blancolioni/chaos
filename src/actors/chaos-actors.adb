with Chaos.Commands.Attacks;
with Chaos.Commands.Meta_Commands;
with Chaos.Commands.Moves;

with Chaos.Actors.Db;

with Chaos.Areas;

with Chaos.Abilities.Able_Objects;

package body Chaos.Actors is

   package Ability_Properties is
     new Chaos.Abilities.Able_Objects (Chaos_Actor_Record);

   --------------------
   -- Add_Properties --
   --------------------

   overriding procedure Add_Properties
     (Object : Chaos_Actor_Record)
   is
   begin
      Ability_Properties.Add_Ability_Properties (Object);
   end Add_Properties;

   ----------
   -- Area --
   ----------

   function Area
     (Actor : Chaos_Actor_Record'Class)
      return access constant Chaos.Areas.Chaos_Area_Record'Class
   is
   begin
      return Actor.Area;
   end Area;

   --------------
   -- Commands --
   --------------

   function Commands
     (Actor : not null access constant Chaos_Actor_Record'Class;
      Environment : not null access constant
        Chaos.Commands.Command_Environment_Interface'Class)
      return Chaos.Commands.Command_Collection
   is
      use Chaos.Commands;
      Actor_Ref : constant Chaos.Actors.Chaos_Actor :=
                    Chaos.Actors.Chaos_Actor (Actor);
   begin
      return Result : Command_Collection do
         Append (Result, Meta_Commands.End_Turn_Command (Actor_Ref));
         Append (Result, Moves.Move_Command (Actor_Ref, Environment));
         Append (Result, Meta_Commands.Wait_Command (Actor_Ref));

         for I in 1 .. Actor.Creature.Power_Count loop
            Append
              (Result,
               Chaos.Commands.Attacks.Power_Attack
                 (Actor_Ref,
                  Actor.Creature.Get_Power (I),
                  Environment));
         end loop;
      end return;
   end Commands;

   ------------------
   -- Create_Actor --
   ------------------

   function Create_Actor
     (From_Creature : Chaos.Creatures.Chaos_Creature;
      Area          : not null access constant
        Chaos.Areas.Chaos_Area_Record'Class;
      Location      : Chaos.Locations.Square_Location;
      Orientation   : Chaos.Locations.Orientation)
      return Chaos_Actor

   is
      procedure Create (Actor : in out Chaos_Actor_Record'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Actor : in out Chaos_Actor_Record'Class) is
      begin
         Actor.Area := Area;
         Actor.Creature := From_Creature;
         Actor.Location := Location;
         Actor.Orientation := Orientation;
         Actor.Hit_Points := From_Creature.Current_Hit_Points;
      end Create;

      Result : constant Chaos_Actor := Db.Create (Create'Access);
   begin
      Area.Add_Actor (Result);
      return Result;
   end Create_Actor;

   --------------
   -- Creature --
   --------------

   function Creature
     (Actor : Chaos_Actor_Record'Class)
      return Chaos.Creatures.Chaos_Creature
   is
   begin
      return Actor.Creature;
   end Creature;

   ------------------------
   -- Current_Hit_Points --
   ------------------------

   function Current_Hit_Points
     (Actor : Chaos_Actor_Record'Class)
      return Natural
   is
   begin
      return Actor.Hit_Points;
   end Current_Hit_Points;

   -----------------------
   -- First_Path_Square --
   -----------------------

   function First_Path_Square
     (Actor : Chaos_Actor_Record'Class)
      return Chaos.Locations.Square_Location
   is
   begin
      return Chaos.Locations.First_Square (Actor.Path);
   end First_Path_Square;

   ----------------------
   -- Has_Minor_Action --
   ----------------------

   function Has_Minor_Action
     (Actor : Chaos_Actor_Record'Class)
      return Boolean
   is
   begin
      return Actor.Minor_Action or else Actor.Minor_Action;
   end Has_Minor_Action;

   ---------------------
   -- Has_Move_Action --
   ---------------------

   function Has_Move_Action
     (Actor : Chaos_Actor_Record'Class)
      return Boolean
   is
   begin
      return Actor.Move_Action or else Actor.Standard_Action;
   end Has_Move_Action;

   --------------
   -- Has_Path --
   --------------

   function Has_Path
     (Actor : Chaos_Actor_Record'Class)
      return Boolean
   is
   begin
      return Chaos.Locations.Length (Actor.Path) > 0;
   end Has_Path;

   -------------------------
   -- Has_Standard_Action --
   -------------------------

   function Has_Standard_Action
     (Actor : Chaos_Actor_Record'Class)
      return Boolean
   is
   begin
      return Actor.Standard_Action;
   end Has_Standard_Action;

   ----------
   -- Kill --
   ----------

   procedure Kill
     (Actor  : Chaos_Actor_Record'Class)
   is
      procedure Kill_Actor
        (Actor_Rec : in out Chaos_Actor_Record'Class);

      ----------------
      -- Kill_Actor --
      ----------------

      procedure Kill_Actor
        (Actor_Rec : in out Chaos_Actor_Record'Class)
      is
      begin
         Actor_Rec.Alive := False;
      end Kill_Actor;

   begin
      Db.Update (Actor.Reference, Kill_Actor'Access);
      if not Actor.Creature.Individual then
         Chaos.Creatures.Update
           (Actor.Creature, Chaos.Creatures.Kill'Access);
      end if;
   end Kill;

   --------------
   -- Location --
   --------------

   function Location
     (Actor : Chaos_Actor_Record'Class)
      return Chaos.Locations.Square_Location
   is
   begin
      return Actor.Location;
   end Location;

   -------------------
   -- Maximum_Shift --
   -------------------

   function Maximum_Shift
     (Actor : Chaos_Actor_Record'Class)
      return Natural
   is
      pragma Unreferenced (Actor);
   begin
      return 1;
   end Maximum_Shift;

   ----------------------
   -- Move_Path_Square --
   ----------------------

   procedure Move_Path_Square
     (Actor : in out Chaos_Actor_Record'Class)
   is
   begin
      Actor.Location := Chaos.Locations.First_Square (Actor.Path);
      Actor.Path := Chaos.Locations.Drop_First (Actor.Path);
      if Actor.Has_Path then
         Actor.Orientation :=
           Chaos.Locations.Get_Direction
             (Actor.Location, Actor.First_Path_Square);
      end if;
   end Move_Path_Square;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Actor_Record)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------------
   -- Orientation --
   -----------------

   function Orientation
     (Actor : Chaos_Actor_Record'Class)
      return Chaos.Locations.Orientation
   is
   begin
      return Actor.Orientation;
   end Orientation;

   ----------
   -- Path --
   ----------

   function Path
     (Actor : Chaos_Actor_Record'Class)
      return Chaos.Locations.Square_Path
   is
   begin
      return Actor.Path;
   end Path;

   -------------------
   -- Reset_Actions --
   -------------------

   procedure Reset_Actions (Actor : in out Chaos_Actor_Record'Class) is
   begin
      Actor.Move_Action := True;
      Actor.Minor_Action := True;
      Actor.Standard_Action := True;
   end Reset_Actions;

   --------------------------
   -- Set_Initial_Location --
   --------------------------

   procedure Set_Initial_Location
     (Actor    : in out Chaos_Actor_Record'Class;
      Location : Chaos.Locations.Square_Location)
   is
   begin
      Actor.Location := Location;
   end Set_Initial_Location;

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (Actor    : in out Chaos_Actor_Record'Class;
      Location : Chaos.Locations.Square_Location)
   is
      Old_Location : constant Chaos.Locations.Square_Location :=
                       Actor.Location;
   begin
      Actor.Location := Location;
      Actor.Area.After_Actor_Moved
        (Db.Reference (Actor), From => Old_Location);
   end Set_Location;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Actor       : in out Chaos_Actor_Record'Class;
      Orientation : Chaos.Locations.Orientation)
   is
   begin
      Actor.Orientation := Orientation;
   end Set_Orientation;

   --------------
   -- Set_Path --
   --------------

   procedure Set_Path
     (Actor : in out Chaos_Actor_Record'Class;
      Path  : Chaos.Locations.Square_Path)
   is
   begin
      Actor.Path := Path;
   end Set_Path;

   -----------
   -- Speed --
   -----------

   overriding function Speed
     (Actor : Chaos_Actor_Record)
      return Chaos.Speed.Chaos_Speed
   is
   begin
      return Actor.Creature.Speed;
   end Speed;

   -----------------
   -- Take_Damage --
   -----------------

   procedure Take_Damage
     (Actor  : Chaos_Actor_Record'Class;
      Form   : Chaos.Powers.Power_Damage_Type;
      Points : Positive)
   is
      pragma Unreferenced (Form);

      procedure Update_Actor_Hit_Points
        (Actor_Rec : in out Chaos_Actor_Record'Class);

      -----------------------------
      -- Update_Actor_Hit_Points --
      -----------------------------

      procedure Update_Actor_Hit_Points
        (Actor_Rec : in out Chaos_Actor_Record'Class)
      is
      begin
         Actor_Rec.Hit_Points := Actor_Rec.Hit_Points - Points;
      end Update_Actor_Hit_Points;

   begin
      Db.Update (Actor.Reference, Update_Actor_Hit_Points'Access);

      if Actor.Creature.Individual then
         declare
            procedure Update_Creature_Hit_Points
              (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class);

            --------------------------------
            -- Update_Creature_Hit_Points --
            --------------------------------

            procedure Update_Creature_Hit_Points
              (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class)
            is
            begin
               Creature.Set_Current_Hit_Points (Actor.Hit_Points);
            end Update_Creature_Hit_Points;

         begin
            Chaos.Creatures.Update
              (Actor.Creature, Update_Creature_Hit_Points'Access);
         end;
      end if;
   end Take_Damage;

   ------------
   -- Update --
   ------------

   procedure Update
     (Actor   : Chaos_Actor_Record'Class;
      Updater : not null access
        procedure (Updated : in out Chaos_Actor_Record'Class))
   is
   begin
      Db.Update (Actor.Reference, Updater);
   end Update;

   ----------------------
   -- Use_Minor_Action --
   ----------------------

   procedure Use_Minor_Action
     (Actor : in out Chaos_Actor_Record'Class)
   is
   begin
      if Actor.Minor_Action then
         Actor.Minor_Action := False;
      elsif Actor.Move_Action then
         Actor.Move_Action := False;
      else
         Actor.Standard_Action := False;
      end if;
   end Use_Minor_Action;

   ---------------------
   -- Use_Move_Action --
   ---------------------

   procedure Use_Move_Action
     (Actor : in out Chaos_Actor_Record'Class)
   is
   begin
      if Actor.Move_Action then
         Actor.Move_Action := False;
      else
         Actor.Standard_Action := False;
      end if;
   end Use_Move_Action;

   -------------------------
   -- Use_Standard_Action --
   -------------------------

   procedure Use_Standard_Action
     (Actor : in out Chaos_Actor_Record'Class)
   is
   begin
      Actor.Standard_Action := False;
   end Use_Standard_Action;

end Chaos.Actors;
