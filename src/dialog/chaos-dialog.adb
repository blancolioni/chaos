with Chaos.Dialog.Db;

with Chaos.Expressions;

with Chaos.Logging;

package body Chaos.Dialog is

   Trace_Dialog : constant Boolean := False;

   ------------
   -- Choice --
   ------------

   function Choice (Position : Dialog_Cursor;
                    Index    : Positive)
                    return Dialog_Cursor
   is
      New_Position : Dialog_Cursor := Position;
   begin
      Choose (New_Position, Index);
      return New_Position;
   end Choice;

   ------------------
   -- Choice_Count --
   ------------------

   function Choice_Count (Position : Dialog_Cursor) return Natural is
   begin
      return Position.Dialog.States.Element (Position.State).Transitions
        .Last_Index;
   end Choice_Count;

   ---------------------
   -- Choice_Has_Text --
   ---------------------

   function Choice_Has_Text
     (Position : Dialog_Cursor;
      Index    : Positive)
      return Boolean
   is
      Transitions      : Dialog_Transition_Index_Vectors.Vector renames
                           Position.Dialog.States.Element (Position.State)
                           .Transitions;
      Transition_Index : constant Natural :=
                           Transitions (Index);
   begin
      return Position.Dialog.Transitions (Transition_Index).Has_Text;
   end Choice_Has_Text;

   -----------------
   -- Choice_Text --
   -----------------

   function Choice_Text
     (Position : Dialog_Cursor;
      Index    : Positive)
      return String
   is
      Transitions      : Dialog_Transition_Index_Vectors.Vector renames
                           Position.Dialog.States.Element (Position.State)
                           .Transitions;
      Transition_Index : constant Natural :=
                           Transitions (Index);
   begin
      return Chaos.Localisation.Indexed_Text
        (Position.Dialog.Transitions (Transition_Index).Transition_Text_Index);
   end Choice_Text;

   ------------
   -- Choose --
   ------------

   procedure Choose (Position : in out Dialog_Cursor;
                     Index    : Positive)
   is
      Transitions      : Dialog_Transition_Index_Vectors.Vector renames
                           Position.Dialog.States.Element (Position.State)
                           .Transitions;
      Transition_Index : constant Natural :=
                           Transitions (Index);
      Transition       : Dialog_Transition renames
                           Position.Dialog.Transitions (Transition_Index);
   begin
      if Transition.Has_Action then
         if Trace_Dialog then
            Chaos.Logging.Log
              ("DIALOG", "action: "
               & Chaos.Expressions.Store.Show (Transition.Action));
         end if;
         Position.Owner.Evaluate (Transition.Action);
      end if;

      if Transition.End_State then
         Position.Owner.On_End_Dialog;
         Position.Finished := True;
      else
         Position.State := Transition.Next_State;
      end if;
   end Choose;

   ---------------
   -- Has_State --
   ---------------

   function Has_State (Position : Dialog_Cursor) return Boolean is
   begin
      return Position.Dialog /= null;
   end Has_State;

   -----------------
   -- Is_Finished --
   -----------------

   function Is_Finished (Position : Dialog_Cursor) return Boolean is
   begin
      return Position.Finished;
   end Is_Finished;

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
      for Transition of Dialog.Transitions loop
         Mark_Value (Transition.Trigger);
         Mark_Value (Transition.Action);
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
      use Lith.Objects;
      State : Natural := 0;
   begin
      while State <= Dialog.States.Last_Index loop
         if Trace_Dialog then
            Chaos.Logging.Log
              ("DIALOG",
               "state" & State'Img & ": "
               & Chaos.Expressions.Store.Show
                 (Dialog.States.Element (State).Trigger));
         end if;

         declare
            Trigger_Result : constant Object :=
                               Owner.Evaluate
                                 (Dialog.States.Element (State).Trigger);
         begin
            exit when Trigger_Result /= False_Value;
         end;

         State := State + 1;
      end loop;

      if State <= Dialog.States.Last_Index then
         declare
            Position : constant Dialog_Cursor :=
                         (Chaos_Dialog (Dialog), State,
                          Chaos.Objects.Chaos_Object (Owner),
                          Finished => False);
         begin
            return Position;
         end;
      else
         return (null, 0, null, True);
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
