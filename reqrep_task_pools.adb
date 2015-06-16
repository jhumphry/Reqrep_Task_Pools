-- reqrep_task_pool.adb
-- A task pool system for simple request-response activity

-- Copyright (c) 2015, James Humphry
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
-- REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
-- AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
-- INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
-- LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
-- OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
-- PERFORMANCE OF THIS SOFTWARE.

pragma Profile(No_Implementation_Extensions);

with Ada.Containers;
use all type Ada.Containers.Count_Type;

with Ada.Containers.Doubly_Linked_Lists;

with Ada.Task_Identification, Ada.Task_Termination;
use all type Ada.Task_Identification.Task_Id;
use all type Ada.Task_Termination.Cause_Of_Termination;

package body Reqrep_Task_Pools is

   ---------------
   -- Task_Pool --
   ---------------

   package body Task_Pool is

      function Status (R : Reqrep_Job) return Reqrep_Status is (R.Status);

      package Work_Queues is new Ada.Containers.Doubly_Linked_Lists
        (Element_Type => Reqrep_Job);
      use all type Work_Queues.List;

      ------------
      -- Queues --
      ------------

      protected Queues is
         procedure Push_Job (R : in Reqrep_Job);
	 procedure Push_Result (R : in Reqrep_Job);
	 entry Push_Exception (R : in Reqrep_Job;
                        E : in Ada.Exceptions.Exception_Occurrence);
         entry Get_Job (R : out Reqrep_Job);
	 entry Get_Result (R : out Reqrep_Job);
	 entry Get_Exception (E : out Ada.Exceptions.Exception_Occurrence);
         procedure Discard_Exception;
         procedure Initiate_Shutdown;
      private
         Input_Queue  : Work_Queues.List := Work_Queues.Empty_List;
	 Output_Queue : Work_Queues.List := Work_Queues.Empty_List;
	 Unhandled_Exception : Boolean := False;
         Unhandled_Occurrence : Ada.Exceptions.Exception_Occurrence;
         In_Shutdown : Boolean := False;
      end Queues;

      protected body Queues is

         procedure Push_Job (R : in Reqrep_Job) is
         begin
            Input_Queue.Append (R);
         end Push_Job;

         procedure Push_Result (R : in Reqrep_Job) is
         begin
            Output_Queue.Append (R);
	 end Push_Result;

	 entry Push_Exception (R : in Reqrep_Job;
                        E : in Ada.Exceptions.Exception_Occurrence)
	   when not Unhandled_Exception is
	 begin
	    Unhandled_Exception := True;
	    Ada.Exceptions.Save_Occurrence(Unhandled_Occurrence, E);
            Output_Queue.Append (R);
	 end Push_Exception;

         entry Get_Job (R : out Reqrep_Job) when In_Shutdown or Input_Queue.Length > 0 is
         begin
            if In_Shutdown then
               R := Reqrep_Job'(Shutdown => True, others => <>);
            else
               R := Input_Queue.First_Element;
               Input_Queue.Delete_First;
            end if;
         end Get_Job;

         entry Get_Result (R : out Reqrep_Job) when Output_Queue.Length > 0 is
         begin
            R := Output_Queue.First_Element;
            Output_Queue.Delete_First;
	 end Get_Result;

	 entry Get_Exception (E : out Ada.Exceptions.Exception_Occurrence)
	   when Unhandled_Exception is
	 begin
            Ada.Exceptions.Save_Occurrence(E, Unhandled_Occurrence);
	    Unhandled_Exception := False;
	 end Get_Exception;

	 procedure Discard_Exception is
	 begin
	    Unhandled_Exception := False;
         end Discard_Exception;

         procedure Initiate_Shutdown is
         begin
            In_Shutdown := True;
         end Initiate_Shutdown;

      end Queues;

      procedure Push_Job
        (R       : in Reqrep;
         Timeout :    Duration := Default_Timeout)
      is
      begin
         Queues.Push_Job
           (Reqrep_Job'
              (R with Status => Ready, Timeout => Timeout, Shutdown => False));
      end Push_Job;

      function Get_Result return Reqrep_Job is
         R : Reqrep_Job;
      begin
         Queues.Get_Result (R);
         return R;
      end Get_Result;

      function Get_Result return Reqrep is
         R : Reqrep_Job;
      begin
         Queues.Get_Result (R);
         return Reqrep (R);
      end Get_Result;

      procedure Get_Exception(E : out Ada.Exceptions.Exception_Occurrence) is
      begin
	 select
	    Queues.Get_Exception(E);
	 else
	    Ada.Exceptions.Save_Occurrence(E, Ada.Exceptions.Null_Occurrence);
	 end select;
      end Get_Exception;

      procedure Discard_Exception is
      begin
	 Queues.Discard_Exception;
      end Discard_Exception;

      procedure Shutdown is
      begin
         Queues.Initiate_Shutdown;
      end Shutdown;

      -------------------
      -- Reqrep_Worker --
      -------------------

      task type Reqrep_Worker is
         entry Start;
      end;

      task body Reqrep_Worker is
         R : Reqrep_Job;
      begin
         accept Start;
         loop
            Queues.Get_Job (R);

            if R.Shutdown then
               exit;
            else

               R.Status := Active;

               select
                  delay R.Timeout;
                  R.Status := Timeout;

               then abort

                  begin
                     R.Status := Execute (Reqrep (R));

                  exception
                     when Event : Inject_Worker_Crash =>
                        raise;
                     when Event : others =>
                        R.Status := Unhandled_Exception;
                        Queues.Push_Exception(R, Event);
                  end;

               end select;

               if R.Status /= Unhandled_Exception then
                  Queues.Push_Result (R);
               end if;
            end if;

         end loop;
      end Reqrep_Worker;

      Workers : array (Positive range 1 .. Number_Workers) of access Reqrep_Worker;

      function Active_Workers return Natural is
         Result : Natural := 0;
      begin
         for I in Workers'Range loop
            if not Ada.Task_Identification.Is_Terminated(Workers(I)'Identity) then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Active_Workers;

      protected body Termination_Handler is

         procedure Handle_Termination(Cause : in Ada.Task_Termination.Cause_Of_Termination;
                                      T : in Ada.Task_Identification.Task_Id;
                                      X : in Ada.Exceptions.Exception_Occurrence) is
         begin
            case Cause is
               when Normal =>
                  null;
               when Abnormal =>
                  Queues.Push_Result(Reqrep_Job'(Status => Internal_Error, others => <>));

               when Unhandled_Exception =>
                  select
                     Queues.Push_Exception(Reqrep_Job'(Status => Internal_Error, others => <>),
                                           X);
                  else
                     Queues.Push_Result(Reqrep_Job'(Status => Internal_Error, others => <>));
                  end select;
            end case;

         end Handle_Termination;

      end Termination_Handler;

   begin
      for I in Workers'Range loop
         Workers(I) := new Reqrep_Worker;
         Ada.Task_Termination.Set_Specific_Handler(Workers(I).all'Identity,
                                                   Termination_Handler_Access);
         Workers(I).Start;
      end loop;
   end Task_Pool;


end Reqrep_Task_Pools;
