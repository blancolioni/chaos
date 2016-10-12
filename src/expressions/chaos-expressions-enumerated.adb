with Ada.Characters.Handling;

with WL.String_Maps;

with Lith.Objects.Symbols;

package body Chaos.Expressions.Enumerated is

   package Enum_Maps is
     new WL.String_Maps (Enum);

   Enum_Values : Enum_Maps.Map;
   Got_Values  : Boolean := False;

   function To_Name (Value : Enum) return String;

   procedure Create_Enum_Values;

   ------------------------
   -- Create_Enum_Values --
   ------------------------

   procedure Create_Enum_Values is
   begin
      for E in Enum loop
         Enum_Values.Insert
           (To_Name (E), E);
      end loop;
      Got_Values := True;
   end Create_Enum_Values;

   -------------
   -- Is_Enum --
   -------------

   function Is_Enum (Value : Lith.Objects.Object) return Boolean is
   begin
      if not Got_Values then
         Create_Enum_Values;
      end if;
      return Lith.Objects.Is_Symbol (Value)
        and then Enum_Values.Contains
          (Lith.Objects.Symbols.Get_Name
             (Lith.Objects.To_Symbol (Value)));
   end Is_Enum;

   -------------
   -- To_Enum --
   -------------

   function To_Enum (Value : Lith.Objects.Object) return Enum is
   begin
      return Enum_Values.Element
        (Lith.Objects.Symbols.Get_Name
           (Lith.Objects.To_Symbol (Value)));
   end To_Enum;

   -------------
   -- To_Name --
   -------------

   function To_Name (Value : Enum) return String is
      Image : String :=
                Ada.Characters.Handling.To_Lower
                  (Enum'Image (Value));
   begin
      for I in Image'Range loop
         if Image (I) = '_' then
            Image (I) := '-';
         end if;
      end loop;
      return Image;
   end To_Name;

   -------------------
   -- To_Expression --
   -------------------

   function To_Object (Value : Enum) return Lith.Objects.Object is
   begin
      if not Got_Values then
         Create_Enum_Values;
      end if;

      return Lith.Objects.To_Object
        (Lith.Objects.Symbols.Get_Symbol (To_Name (Value)));
   end To_Object;

begin
   Create_Enum_Values;
end Chaos.Expressions.Enumerated;
