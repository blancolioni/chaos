with Lith.Objects.Symbols;

with Chaos.Paths;

package body Chaos.Expressions.Import.Objects is

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
      pragma Unreferenced (Team, Faction, EA, General, Race, Class,
                           Specific, Gender, Alignment,
                           Id_1, Id_2, Id_3, Id_4, Id_5);
   begin
      if Name /= "" then
         Store.Push
           (Lith.Objects.Symbols.Get_Symbol ("chaos-object-with-code"));
         Store.Push (Lith.Objects.Symbols.Quote_Symbol);
         Store.Push (Lith.Objects.Symbols.Get_Symbol (Name));
         Store.Create_List (2);
         Store.Create_List (2);
         Store.Push (Store.Pop, Lith.Objects.Secondary);
      else
         Store.Push (Lith.Objects.Symbols.Quote_Symbol);
         Store.Push_Nil;
         Store.Create_List (2);
         Store.Push (Store.Pop, Lith.Objects.Secondary);
      end if;
   end Import_Object;

   ------------------------------
   -- Import_Object_Identifier --
   ------------------------------

   procedure Import_Object_Identifier
     (Id : String)
   is
   begin
      Store.Push (Lith.Objects.Symbols.Get_Symbol ("chaos-get-" & Id));
      Store.Push (Lith.Objects.Symbols.Get_Symbol ("this"));
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
        (Lith.Objects.Symbols.Get_Symbol ("chaos-object-with-code"));
      Store.Push (Lith.Objects.Symbols.Quote_Symbol);
      Store.Push (Lith.Objects.Symbols.Get_Symbol (Name));
      Store.Create_List (2);
      Store.Create_List (2);
      Store.Push (Store.Pop, Lith.Objects.Secondary);
   end Import_Object_Name;

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
