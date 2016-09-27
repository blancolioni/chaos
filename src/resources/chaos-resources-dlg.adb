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
            Dlg.State_Table.Append (State);
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

end Chaos.Resources.Dlg;
