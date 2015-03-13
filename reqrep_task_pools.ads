-- reqrep_task_pool.ads
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

package Reqrep_Task_Pools is

   Default_Timeout : constant Duration := 60.0;

   type Reqrep_Status is
     (Ready, Enqueued, Active, Success, Failure, Unhandled_Exception, Timeout);
   subtype Reqrep_Return_Status is Reqrep_Status range Success .. Failure;

   generic
      type Reqrep is tagged private;
      with function Execute
        (R : in out Reqrep) return Reqrep_Return_Status is <>;
      Number_Workers : Positive := 1;
   package Task_Pool is
      type Reqrep_Job is new Reqrep with private;
      function Status (R : Reqrep_Job) return Reqrep_Status;
      procedure Push_Job
        (R       : in Reqrep;
         Timeout :    Duration := Default_Timeout);
      function Get_Result return Reqrep_Job;
      function Get_Result return Reqrep;
      procedure Shutdown;

   private
      type Reqrep_Job is new Reqrep with record
         Status   : Reqrep_Status;
         Timeout  : Duration;
         Shutdown : Boolean := False;
      end record;
   end Task_Pool;

end Reqrep_Task_Pools;
