with Chaos.Paths;

package body Chaos.Expressions.Import.Objects is

   ----------------
   -- Group_Name --
   ----------------

   function Group_Name
     (Tuple_Index : Object_Tuple_Index)
      return String
   is
   begin
      case Tuple_Index is
         when 1 =>
            return "ea";
         when 2 =>
            return "general";
         when 3 =>
            return "race";
         when 4 =>
            return "class";
         when 5 =>
            return "specific";
         when 6 =>
            return "gender";
         when 7 =>
            return "alignment";
      end case;
   end Group_Name;

   -------------------
   -- Import_Object --
   -------------------

   procedure Import_Object
     (Team      : Natural;
      Faction   : Natural;
      EA        : Natural;
      General   : Natural;
      Race      : Natural;
      Class     : Natural;
      Specific  : Natural;
      Gender    : Natural;
      Alignment : Natural;
      Id_1      : Natural;
      Id_2      : Natural;
      Id_3      : Natural;
      Id_4      : Natural;
      Id_5      : Natural;
      Name      : String)
   is
      pragma Unreferenced (Team, Faction,
                           Id_1, Id_2, Id_3, Id_4, Id_5);
   begin
      if Name /= "" then
         Store.Push
           ("chaos-object-with-code");
         Store.Push (Lith.Objects.Single_Quote);
         Store.Push (Name);
         Store.Create_List (2);
         Store.Create_List (2);
         Store.Push (Store.Pop, Lith.Objects.Secondary);
      else
         Import_Object_Tuple
           ((EA, General, Race, Class, Specific, Gender, Alignment));
      end if;
   end Import_Object;

   ------------------------------
   -- Import_Object_Identifier --
   ------------------------------

   procedure Import_Object_Identifier
     (Id : String)
   is
   begin
      Store.Push ("chaos-get-" & Id);
      Store.Push ("this");
      Store.Create_List (2);
      Store.Push (Store.Pop, Lith.Objects.Secondary);
   end Import_Object_Identifier;

   ------------------------
   -- Import_Object_Name --
   ------------------------

   procedure Import_Object_Name
     (Name : String)
   is
   begin
      Store.Push
        ("chaos-object-with-code");
      Store.Push (Lith.Objects.Single_Quote);
      Store.Push (Name);
      Store.Create_List (2);
      Store.Create_List (2);
      Store.Push (Store.Pop, Lith.Objects.Secondary);
   end Import_Object_Name;

   -------------------------
   -- Import_Object_Tuple --
   -------------------------

   procedure Import_Object_Tuple
     (Tuple : Script_Tuple)
   is
   begin
      if (for all X of Tuple => X = 0) then
         Store.Push (Lith.Objects.False_Value, Lith.Objects.Secondary);
         return;
      end if;

      Store.Push ("chaos-match-object");
      for I in 1 .. 7 loop
         if I <= Tuple'Last then
            Store.Push (Tuple (I));
         else
            Store.Push (0);
         end if;
      end loop;
      Store.Create_List (8);
      Store.Push (Store.Pop, Lith.Objects.Secondary);
   end Import_Object_Tuple;

   ------------------
   -- Load_Objects --
   ------------------

   procedure Load_Objects is
   begin
      if not Chaos.Expressions.Store.Load
        (Chaos.Paths.Config_File
           ("script/objects.scm"))
      then
         raise Constraint_Error with
           "cannot load object configuration";
      end if;
   end Load_Objects;

end Chaos.Expressions.Import.Objects;
