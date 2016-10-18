with WL.Binary_IO;

with Chaos.Logging;

with Chaos.Parser;

with Chaos.Dialog.Db;

with Chaos.Resources.Dlg;
with Chaos.Resources.Manager;

package body Chaos.Dialog.Import is

   -------------------
   -- Import_Dialog --
   -------------------

   function Import_Dialog
     (Name : String)
      return Chaos_Dialog
   is
      Res : constant access constant Chaos.Resources.Chaos_Resource'Class :=
              Chaos.Resources.Manager.Load_Resource
                (Reference => Chaos.Resources.To_Reference (Name),
                 Res_Type  => Chaos.Resources.Dialog_Resource);

   begin
      if Res /= null then
         declare
            Dlg : Chaos.Resources.Dlg.Dlg_Resource'Class renames
                    Chaos.Resources.Dlg.Dlg_Resource'Class (Res.all);

            procedure Create (Dialog : in out Chaos_Dialog_Record'Class)
            is null;

            procedure Configure (Dialog : in out Chaos_Dialog_Record'Class);

            ---------------
            -- Configure --
            ---------------

            procedure Configure (Dialog : in out Chaos_Dialog_Record'Class) is
               use type WL.Binary_IO.Word_32;
            begin
               Dialog.Initialize (Name);
               for I in 0 .. Dlg.State_Count - 1 loop
                  declare
                     State : Dialog_State;
                  begin
                     State.Response_Index :=
                       Chaos.Localisation.Local_Text_Index
                         (Dlg.State_Response (I));

                     declare
                        Trigger : constant String :=
                                    Dlg.State_Trigger (I);
                     begin
                        if Trigger /= "" then
                           Chaos.Logging.Log
                             ("DIALOG",
                              Name & " state" & I'Img & " trigger: "
                              & Trigger);
                           State.Trigger :=
                             Chaos.Parser.Parse_Trigger (Trigger);
                        else
                           State.Trigger := Lith.Objects.True_Value;
                        end if;
                     end;

                     for J in 0 .. Dlg.State_Transition_Count (I) - 1 loop
                        State.Transitions.Append
                          (Natural (Dlg.State_Transition_Index (I, J)));
                     end loop;

                     Dialog.States.Append (State);
                  end;
               end loop;

               for I in 0 .. Dlg.Transition_Count - 1 loop
                  declare
                     Transition  : Dialog_Transition;
                     Flags       : constant WL.Binary_IO.Word_32 :=
                                     Dlg.Transition_Flags (I);
                  begin
                     Transition.Has_Text := (Flags and 1) /= 0;
                     Transition.Has_Trigger := (Flags and 2) /= 0;
                     Transition.Has_Action := (Flags and 4) /= 0;
                     Transition.End_State := (Flags and 8) /= 0;
                     if Transition.Has_Text then
                        Transition.Transition_Text_Index :=
                          Chaos.Localisation.Local_Text_Index
                            (Dlg.Transition_Text (I));
                        if False then
                           Chaos.Logging.Log
                             ("DIALOG",
                              Chaos.Localisation.Indexed_Text
                                (Transition.Transition_Text_Index));
                        end if;
                     end if;
                     if Transition.Has_Trigger then
                        Transition.Trigger :=
                          Chaos.Parser.Parse_Trigger
                            (Dlg.Transition_Trigger (I));
                     end if;
                     if Transition.Has_Action then
                        Transition.Action :=
                          Chaos.Parser.Parse_Action
                            (Dlg.Transition_Action (I));
                     end if;
                     if not Transition.End_State then
                        Transition.Next_State :=
                          Natural
                            (Dlg.Transition_Next_State (I));
                     end if;
                     Dialog.Transitions.Append (Transition);
                  end;
               end loop;
            end Configure;

            Dialog : constant Chaos_Dialog :=
                       Db.Create (Create'Access);
         begin
            Dialog.Save_Object;
            Db.Update (Dialog.Reference, Configure'Access);
            return Dialog;
         end;
      else
         return null;
      end if;

   end Import_Dialog;

end Chaos.Dialog.Import;
