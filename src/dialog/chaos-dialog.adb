with Chaos.Dialog.Db;

package body Chaos.Dialog is

   ------------
   -- Choice --
   ------------

   function Choice (Position : Dialog_Cursor;
                    Index    : Positive)
                    return Dialog_Cursor
   is
      Transitions : Dialog_Transition_Vectors.Vector renames
                      Position.Dialog.States.Element (Position.State)
                      .Transitions;
   begin
      return (Position.Dialog, Transitions.Element (Index).Next_State);
   end Choice;

   ------------------
   -- Choice_Count --
   ------------------

   function Choice_Count (Position : Dialog_Cursor) return Natural is
   begin
      return Position.Dialog.States.Element (Position.State).Transitions
        .Last_Index;
   end Choice_Count;

   -----------------
   -- Choice_Text --
   -----------------

   function Choice_Text
     (Position : Dialog_Cursor;
      Index    : Positive)
      return String
   is
      Transitions : Dialog_Transition_Vectors.Vector renames
                      Position.Dialog.States.Element (Position.State)
                      .Transitions;
   begin
      return Chaos.Localisation.Indexed_Text
        (Transitions.Element (Index).Transition_Text_Index);
   end Choice_Text;

   ------------
   -- Choose --
   ------------

   procedure Choose (Position : in out Dialog_Cursor;
                     Index    : Positive)
   is
      Transitions : Dialog_Transition_Vectors.Vector renames
                      Position.Dialog.States.Element (Position.State)
                      .Transitions;
   begin
      Position.State := Transitions.Element (Index).Next_State;
   end Choose;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Dialog_Record)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------
   -- Start --
   -----------

   function Start (Dialog : not null access constant
                     Chaos_Dialog_Record'Class)
                   return Dialog_Cursor
   is
   begin
      return (Chaos_Dialog (Dialog), 0);
   end Start;

   ----------
   -- Text --
   ----------

   function Text (Position : Dialog_Cursor) return String is
   begin
      return Chaos.Localisation.Indexed_Text
        (Position.Dialog.States.Element (Position.State).Response_Index);
   end Text;

end Chaos.Dialog;
