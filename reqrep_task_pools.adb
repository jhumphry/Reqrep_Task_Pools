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

with Ada.Containers;
use all type Ada.Containers.Count_Type;

with Ada.Containers.Doubly_Linked_Lists;

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
	 entry Push_Result (R : in Reqrep_Job);
	 entry Push_Exception (R : in Reqrep_Job;
			E : in Ada.Exceptions.Exception_Occurrence);
         entry Get_Job (R : out Reqrep_Job);
	 entry Get_Result (R : out Reqrep_Job);
	 entry Get_Exception (E : out Ada.Exceptions.Exception_Occurrence);
	 procedure Discard_Exception;
      private
         Input_Queue  : Work_Queues.List := Work_Queues.Empty_List;
	 Output_Queue : Work_Queues.List := Work_Queues.Empty_List;
	 Unhandled_Exception : Boolean := False;
	 Unhandled_Occurrence : Ada.Exceptions.Exception_Occurrence;
      end Queues;

      protected body Queues is

         procedure Push_Job (R : in Reqrep_Job) is
         begin
            Input_Queue.Append (R);
         end Push_Job;

         entry Push_Result (R : in Reqrep_Job) when not Unhandled_Exception is
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

         entry Get_Job (R : out Reqrep_Job) when Input_Queue.Length > 0 is
         begin
            R := Input_Queue.First_Element;
            Input_Queue.Delete_First;
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

      -------------------
      -- Reqrep_Worker --
      -------------------

      task type Reqrep_Worker;

      task body Reqrep_Worker is
         R : Reqrep_Job;
      begin
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

      Workers : array (Positive range 1 .. Number_Workers) of Reqrep_Worker;

      procedure Shutdown is
      begin
         for I in Workers'Range loop
            Queues.Push_Job
              (Reqrep_Job'
                 (Reqrep with
                  Status   => Ready,
                  Timeout  => Default_Timeout,
                  Shutdown => True));
         end loop;
      end Shutdown;

   end Task_Pool;

end Reqrep_Task_Pools;
