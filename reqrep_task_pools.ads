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

pragma Profile(No_Implementation_Extensions);

with Ada.Exceptions;
with Ada.Task_Termination;
with Ada.Task_Identification;

package Reqrep_Task_Pools is

   Default_Timeout : constant Duration := 60.0;

   type Reqrep_Status is
     (Ready, Enqueued, Active, Success, Failure, Unhandled_Exception, Timeout, Internal_Error);
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
      function Get_Result return Reqrep_Job
        with Post => not (Status(Get_Result'Result) in Ready..Active);
      function Get_Result return Reqrep;
      procedure Get_Exception(E : out Ada.Exceptions.Exception_Occurrence);
      procedure Discard_Exception;
      procedure Shutdown;
      function Active_Workers return Natural;

   private
      type Reqrep_Job is new Reqrep with record
         Status   : Reqrep_Status;
         Timeout  : Duration;
         Shutdown : Boolean := False;
      end record;

      protected Termination_Handler is
         procedure Handle_Termination(Cause : in Ada.Task_Termination.Cause_Of_Termination;
                                      T : in Ada.Task_Identification.Task_Id;
                                      X : in Ada.Exceptions.Exception_Occurrence);
      end Termination_Handler;

      Termination_Handler_Access : constant access protected procedure
     (Cause : in Ada.Task_Termination.Cause_Of_Termination;
      T     : in Ada.Task_Identification.Task_Id;
      X     : in Ada.Exceptions.Exception_Occurrence)
        := Termination_Handler.Handle_Termination'Access;

   end Task_Pool;

   -- This exception is only used to test whether workers are restarted upon failure
   Inject_Worker_Crash : exception;

end Reqrep_Task_Pools;
