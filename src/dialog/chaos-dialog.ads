private with Ada.Containers.Vectors;

private with Memor;

private with Chaos.Expressions;
private with Chaos.Localisation;

with Chaos.Objects;

package Chaos.Dialog is

   type Chaos_Dialog_Record is
     new Chaos.Objects.Root_Chaos_Object_Record
   with private;

   type Chaos_Dialog is access constant Chaos_Dialog_Record'Class;

   type Dialog_Cursor is private;

   function Start (Dialog : not null access constant
                     Chaos_Dialog_Record'Class)
                   return Dialog_Cursor;

   function Text (Position : Dialog_Cursor) return String;

   function Choice_Count (Position : Dialog_Cursor) return Natural;

   function Choice_Text
     (Position : Dialog_Cursor;
      Index    : Positive)
      return String;

   function Choice (Position : Dialog_Cursor;
                    Index    : Positive)
                    return Dialog_Cursor;

   procedure Choose (Position : in out Dialog_Cursor;
                     Index    : Positive);

private

   type Dialog_Cursor is
      record
         Dialog : Chaos_Dialog;
         State  : Natural;
      end record;

   type Dialog_Transition is
      record
         Has_Text              : Boolean := False;
         Has_Trigger           : Boolean := False;
         Has_Action            : Boolean := False;
         End_State             : Boolean := False;
         Has_Journal           : Boolean := False;
         Has_Quest             : Boolean := False;
         Remove_Quest          : Boolean := False;
         Done_Quest            : Boolean := False;
         Transition_Text_Index : Chaos.Localisation.Local_Text_Index;
         Journal_Text_Index    : Chaos.Localisation.Local_Text_Index;
         Trigger               : Chaos.Expressions.Chaos_Expression;
         Action                : Chaos.Expressions.Chaos_Expression;
         Next_State            : Natural;
      end record;

   package Dialog_Transition_Vectors is
      new Ada.Containers.Vectors (Natural, Dialog_Transition);

   type Dialog_State is
      record
         Response_Index : Chaos.Localisation.Local_Text_Index;
         Transitions    : Dialog_Transition_Vectors.Vector;
      end record;

   package Dialog_State_Vectors is
     new Ada.Containers.Vectors (Natural, Dialog_State);

   type Chaos_Dialog_Record is
     new Chaos.Objects.Root_Chaos_Object_Record with
      record
         States : Dialog_State_Vectors.Vector;
      end record;

   overriding function Object_Database
     (Object : Chaos_Dialog_Record)
      return Memor.Root_Database_Type'Class;

end Chaos.Dialog;
