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

with Ada.Containers
  .Synchronized_Queue_Interfaces, Ada.Containers
  .Unbounded_Synchronized_Queues;

package body Reqrep_Task_Pool is

   ---------------
   -- Task_Pool --
   ---------------

   package body Task_Pool is

      function Status (R : Reqrep_Job) return Reqrep_Status is (R.Status);

      package SQI is new Ada.Containers.Synchronized_Queue_Interfaces
        (Element_Type => Reqrep_Job);
      package Work_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
        (Queue_Interfaces => SQI);

      Input_Queue  : Work_Queues.Queue;
      Output_Queue : Work_Queues.Queue;

      procedure Push (R : in Reqrep; Timeout : Duration := Default_Timeout) is
      begin
         Input_Queue.Enqueue
           (Reqrep_Job'
              (R with Status => Ready, Timeout => Timeout, Shutdown => False));
      end Push;

      function Get_Result return Reqrep_Job is
         R : Reqrep_Job;
      begin
         Output_Queue.Dequeue (R);
         return R;
      end Get_Result;

      function Get_Result return Reqrep is
         R : Reqrep_Job;
      begin
         Output_Queue.Dequeue (R);
         return Reqrep (R);
      end Get_Result;

      -------------------
      -- Reqrep_Worker --
      -------------------

      task type Reqrep_Worker;

      task body Reqrep_Worker is
         R : Reqrep_Job;
      begin
         loop
            Input_Queue.Dequeue (R);

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
                     if R.Status = Active then
                        R.Status := Success;
                     end if;

                  exception
                     when Event : others =>
                        R.Status := Unhandled_Exception;
                  end;

               end select;

               Output_Queue.Enqueue (R);
            end if;
         end loop;
      end Reqrep_Worker;

      Workers : array (Positive range 1 .. Number_Workers) of Reqrep_Worker;

      procedure Shutdown is
      begin
         for I in Workers'Range loop
            Input_Queue.Enqueue
              (Reqrep_Job'
                 (Reqrep with
                  Status   => Ready,
                  Timeout  => Default_Timeout,
                  Shutdown => True));
         end loop;
      end Shutdown;

   end Task_Pool;

end Reqrep_Task_Pool;
