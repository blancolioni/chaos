private with Ada.Containers.Vectors;

package Chaos.Resources.Dlg is

   type Dlg_Resource is
     new Chaos_Resource with private;

   function State_Count
     (Dlg : Dlg_Resource'Class)
      return WL.Binary_IO.Word_32;

   function State_Response
     (Dlg   : Dlg_Resource'Class;
      State : WL.Binary_IO.Word_32)
      return WL.Binary_IO.Word_32;

   function State_Transition_Count
     (Dlg : Dlg_Resource'Class;
      State : WL.Binary_IO.Word_32)
      return WL.Binary_IO.Word_32;

   function State_Transition_Index
     (Dlg        : Dlg_Resource'Class;
      State      : WL.Binary_IO.Word_32;
      Transition : WL.Binary_IO.Word_32)
      return WL.Binary_IO.Word_32;

   function Transition_Flags
     (Dlg        : Dlg_Resource'Class;
      Transition : WL.Binary_IO.Word_32)
      return WL.Binary_IO.Word_32;

   function Transition_Text
     (Dlg        : Dlg_Resource'Class;
      Transition : WL.Binary_IO.Word_32)
      return WL.Binary_IO.Word_32;

   function Transition_Next_State
     (Dlg        : Dlg_Resource'Class;
      Transition : WL.Binary_IO.Word_32)
      return WL.Binary_IO.Word_32;

private

   type State_Table_Entry is
      record
         Actor_Reponse    : Word_32;
         First_Transition : Word_32;
         Transition_Count : Word_32;
         State_Trigger    : Word_32;
      end record;

   package State_Table_Vectors is
     new Ada.Containers.Vectors (Natural, State_Table_Entry);

   type Transition_Table_Entry is
      record
         Flags                : Word_32;
         Text                 : Word_32;
         Journal_Text         : Word_32;
         Trigger_Index        : Word_32;
         Action_Index         : Word_32;
         Next_State_Reference : Resource_Reference;
         Next_State_Index     : Word_32;
      end record;

   package Transition_Table_Vectors is
     new Ada.Containers.Vectors (Natural, Transition_Table_Entry);

   type Dlg_Resource is
     new Chaos_Resource with
      record
         State_Count               : Word_32;
         State_Offset              : Word_32;
         Transition_Count          : Word_32;
         Transition_Offset         : Word_32;
         State_Trigger_Offset      : Word_32;
         State_Trigger_Count       : Word_32;
         Transition_Trigger_Offset : Word_32;
         Transition_Trigger_Count  : Word_32;
         Action_Count              : Word_32;
         Action_Offset             : Word_32;
         Flags                     : Word_32;
         State_Table               : State_Table_Vectors.Vector;
         Transition_Table          : Transition_Table_Vectors.Vector;
      end record;

   overriding function Signature
     (Dlg : Dlg_Resource)
      return String
   is ("DLG ");

   overriding procedure Load
     (Dlg : in out Dlg_Resource);

end Chaos.Resources.Dlg;
