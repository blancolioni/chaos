package body Chaos.Resources.Dlg is

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Dlg : in out Dlg_Resource)
   is
   begin
      Dlg.Set_Offset (8);
      Dlg.Get (Dlg.State_Count);
      Dlg.Get (Dlg.State_Offset);
      Dlg.Get (Dlg.Transition_Count);
      Dlg.Get (Dlg.Transition_Offset);
      Dlg.Get (Dlg.State_Trigger_Offset);
      Dlg.Get (Dlg.State_Trigger_Count);
      Dlg.Get (Dlg.Transition_Trigger_Offset);
      Dlg.Get (Dlg.Transition_Trigger_Count);
      Dlg.Get (Dlg.Action_Offset);
      Dlg.Get (Dlg.Action_Count);

      Dlg.Set_Offset (Dlg.State_Offset);
      for State_Index in 0 .. Dlg.State_Count - 1 loop
         declare
            State : State_Table_Entry;
         begin
            Dlg.Get (State.Actor_Reponse);
            Dlg.Get (State.First_Transition);
            Dlg.Get (State.Transition_Count);
            Dlg.Get (State.State_Trigger);
            if State.State_Trigger /= 16#FFFF_FFFF# then
               Dlg.Push_Offset
                 (Dlg.State_Trigger_Offset + 8 * State.State_Trigger);
               declare
                  Offset, Length : Word_32;
               begin
                  Dlg.Get (Offset);
                  Dlg.Get (Length);
                  Dlg.Push_Offset (Offset);
                  State.Trigger :=
                    Ada.Strings.Unbounded.To_Unbounded_String
                      (Dlg.Get (Natural (Length)));
                  Dlg.Pop_Offset;
               end;
               Dlg.Pop_Offset;
            end if;
            declare
               Index : Integer := Dlg.State_Table.Last_Index;
            begin
               Dlg.State_Table.Append (State);
               while Index >= 0
                 and then State.State_Trigger <
                   Dlg.State_Table.Element (Index).State_Trigger
               loop
                  Dlg.State_Table (Index + 1) :=
                    Dlg.State_Table (Index);
                  Index := Index - 1;
               end loop;
               Dlg.State_Table (Index + 1) := State;
            end;
         end;
      end loop;

      Dlg.Set_Offset (Dlg.Transition_Offset);
      for State_Index in 0 .. Dlg.Transition_Count - 1 loop
         declare
            Transition : Transition_Table_Entry;
         begin
            Dlg.Get (Transition.Flags);
            Dlg.Get (Transition.Text);
            Dlg.Get (Transition.Journal_Text);
            Dlg.Get (Transition.Trigger_Index);
            Dlg.Get (Transition.Action_Index);
            Dlg.Get (Transition.Next_State_Reference);
            Dlg.Get (Transition.Next_State_Index);

            if (Transition.Flags and 2) = 2 then
               Dlg.Push_Offset
                 (Dlg.Transition_Trigger_Offset
                  + 8 * Transition.Trigger_Index);
               declare
                  Offset, Length : Word_32;
               begin
                  Dlg.Get (Offset);
                  Dlg.Get (Length);
                  Dlg.Push_Offset (Offset);
                  Transition.Trigger :=
                    Ada.Strings.Unbounded.To_Unbounded_String
                      (Dlg.Get (Natural (Length)));
                  Dlg.Pop_Offset;
               end;
               Dlg.Pop_Offset;
            end if;

            if (Transition.Flags and 4) = 4 then
               Dlg.Push_Offset
                 (Dlg.Action_Offset
                  + 8 * Transition.Action_Index);
               declare
                  Offset, Length : Word_32;
               begin
                  Dlg.Get (Offset);
                  Dlg.Get (Length);
                  Dlg.Push_Offset (Offset);
                  Transition.Action :=
                    Ada.Strings.Unbounded.To_Unbounded_String
                      (Dlg.Get (Natural (Length)));
                  Dlg.Pop_Offset;
               end;
               Dlg.Pop_Offset;
            end if;

            Dlg.Transition_Table.Append (Transition);
         end;
      end loop;

   end Load;

   -----------------
   -- State_Count --
   -----------------

   function State_Count
     (Dlg : Dlg_Resource'Class)
      return WL.Binary_IO.Word_32
   is
   begin
      return Dlg.State_Count;
   end State_Count;

   --------------------
   -- State_Response --
   --------------------

   function State_Response
     (Dlg   : Dlg_Resource'Class;
      State : WL.Binary_IO.Word_32)
      return WL.Binary_IO.Word_32
   is
   begin
      return Dlg.State_Table.Element (Natural (State)).Actor_Reponse;
   end State_Response;

   ----------------------------
   -- State_Transition_Count --
   ----------------------------

   function State_Transition_Count
     (Dlg   : Dlg_Resource'Class;
      State : WL.Binary_IO.Word_32)
      return WL.Binary_IO.Word_32
   is
   begin
      return Dlg.State_Table.Element (Natural (State)).Transition_Count;
   end State_Transition_Count;

   ----------------------------
   -- State_Transition_Index --
   ----------------------------

   function State_Transition_Index
     (Dlg        : Dlg_Resource'Class;
      State      : WL.Binary_IO.Word_32;
      Transition : WL.Binary_IO.Word_32)
      return WL.Binary_IO.Word_32
   is
   begin
      return Dlg.State_Table.Element (Natural (State)).First_Transition
        + Transition;
   end State_Transition_Index;

   -------------------
   -- State_Trigger --
   -------------------

   function State_Trigger
     (Dlg   : Dlg_Resource'Class;
      State : WL.Binary_IO.Word_32)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String
        (Dlg.State_Table.Element (Natural (State)).Trigger);
   end State_Trigger;

   -----------------------
   -- Transition_Action --
   -----------------------

   function Transition_Action
     (Dlg        : Dlg_Resource'Class;
      Transition : WL.Binary_IO.Word_32)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String
        (Dlg.Transition_Table.Element (Natural (Transition)).Action);
   end Transition_Action;

   ----------------------
   -- Transition_Count --
   ----------------------

   function Transition_Count
     (Dlg  : Dlg_Resource'Class)
      return WL.Binary_IO.Word_32
   is
   begin
      return Dlg.Transition_Count;
   end Transition_Count;

   ----------------------
   -- Transition_Flags --
   ----------------------

   function Transition_Flags
     (Dlg        : Dlg_Resource'Class;
      Transition : WL.Binary_IO.Word_32)
      return WL.Binary_IO.Word_32
   is
   begin
      return Dlg.Transition_Table.Element (Natural (Transition)).Flags;
   end Transition_Flags;

   ---------------------------
   -- Transition_Next_State --
   ---------------------------

   function Transition_Next_State
     (Dlg        : Dlg_Resource'Class;
      Transition : WL.Binary_IO.Word_32)
      return WL.Binary_IO.Word_32
   is
   begin
      return Dlg.Transition_Table.Element (Natural (Transition))
        .Next_State_Index;
   end Transition_Next_State;

   ---------------------
   -- Transition_Text --
   ---------------------

   function Transition_Text
     (Dlg        : Dlg_Resource'Class;
      Transition : WL.Binary_IO.Word_32)
      return WL.Binary_IO.Word_32
   is
   begin
      return Dlg.Transition_Table.Element (Natural (Transition)).Text;
   end Transition_Text;

   ------------------------
   -- Transition_Trigger --
   ------------------------

   function Transition_Trigger
     (Dlg        : Dlg_Resource'Class;
      Transition : WL.Binary_IO.Word_32)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String
        (Dlg.Transition_Table.Element (Natural (Transition)).Trigger);
   end Transition_Trigger;

end Chaos.Resources.Dlg;
