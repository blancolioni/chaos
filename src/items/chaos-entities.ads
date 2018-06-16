private with WL.Binary_IO;

private with Chaos.Localisation;

with Chaos.Coins;
with Chaos.Weight;

with Chaos.Equipment;
with Chaos.Objects;

package Chaos.Entities is

   type Chaos_Entity_Record is
     abstract new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Equipment.Chaos_Equippable_Interface
   with private;

   type Chaos_Entity is access constant Chaos_Entity_Record'Class;

   overriding function Equipment_Slot_OK
     (Entity : Chaos_Entity_Record;
      Slot : Chaos.Equipment.Chaos_Equipment_Slot)
      return Boolean;

   function Weight
     (Entity : Chaos_Entity_Record)
      return Chaos.Weight.Chaos_Weight;

   function Price
     (Entity : Chaos_Entity_Record)
      return Chaos.Coins.Coins_Type;

private

   type Chaos_Entity_Record is
     abstract new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Equipment.Chaos_Equippable_Interface with
      record
         Needs_Identification : Boolean := False;
         Identified_Name      : Chaos.Localisation.Local_Text_Index;
         Unidentified_Name    : Chaos.Localisation.Local_Text_Index;
         Identified_Desc      : Chaos.Localisation.Local_Text_Index;
         Unidentified_Desc    : Chaos.Localisation.Local_Text_Index;
         Price                : Chaos.Coins.Coins_Type;
         Weight               : Chaos.Weight.Chaos_Weight;
      end record;

   overriding procedure Add_Properties
     (Entity : Chaos_Entity_Record);

   procedure Register_Entity
     (Code   : String;
      Entity : Chaos_Entity);

   function Exists
     (Code : String)
      return Boolean;

   function Get
     (Code : String)
      return Chaos_Entity;

   procedure Initialize
     (Entity               : in out Chaos_Entity_Record'Class;
      Code                 : String;
      Needs_Identification : Boolean;
      Identified_Name      : WL.Binary_IO.Word_32;
      Unidentified_Name    : WL.Binary_IO.Word_32;
      Identified_Desc      : WL.Binary_IO.Word_32;
      Unidentified_Desc    : WL.Binary_IO.Word_32);

end Chaos.Entities;
