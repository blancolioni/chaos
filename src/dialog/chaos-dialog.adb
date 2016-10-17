with Lith.Objects.Symbols;

with Chaos.Dialog.Db;

with Chaos.Expressions;

with Chaos.Logging;

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
      return (Position.Dialog, Transitions.Element (Index).Next_State,
              Position.Owner);
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
      if Finished (Position) then
         Position.Owner.On_End_Dialog;
      end if;
   end Choose;

   --------------
   -- Finished --
   --------------

   function Finished (Position : Dialog_Cursor) return Boolean is
   begin
      return Choice_Count (Position) = 0;
   end Finished;

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (Dialog     : in out Chaos_Dialog_Record;
      Mark_Value : not null access
        procedure (Value : in out Lith.Objects.Object))
   is
   begin
      Chaos.Objects.Root_Chaos_Object_Record (Dialog).Mark (Mark_Value);
      for State of Dialog.States loop
         Mark_Value (State.Trigger);
      end loop;
   end Mark;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Dialog_Record)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------
   -- Start --
   -----------

   function Start
     (Dialog : not null access constant
        Chaos_Dialog_Record'Class;
      Owner  : not null access constant
        Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Dialog_Cursor
   is
      use Lith.Objects, Chaos.Expressions;
      State : Natural := 0;
   begin
      while State <= Dialog.States.Last_Index loop
         Chaos.Logging.Log
           ("DIALOG",
            Store.Show (Dialog.States.Element (State).Trigger));
         exit when Chaos.Expressions.Store.Evaluate
           (Dialog.States.Element (State).Trigger,
            Symbols.Get_Symbol ("this"), Owner.To_Expression)
           /= False_Value;
         State := State + 1;
      end loop;

      if State <= Dialog.States.Last_Index then
         declare
            Position : constant Dialog_Cursor :=
                         (Chaos_Dialog (Dialog), State,
                          Chaos.Objects.Chaos_Object (Owner));
         begin
            if Finished (Position) then
               Owner.On_End_Dialog;
            end if;
            return Position;
         end;
      else
         return (null, 0, null);
      end if;
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
